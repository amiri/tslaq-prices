{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TSLAQPrices
  ( updatePrices
  , getLatestJSONFileRemote
  , readJSONFileRemote
  , localPricesFolder
  , importAlgoSeek
  , saved
  , emptyPrices
  , tslaqPricesBucket
  ) where

import           Control.Lens
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask)
import           Data.Aeson                          (decode, encode)
import qualified Data.ByteString.Lazy                as B (ByteString,
                                                           writeFile)
import           Data.Conduit.Combinators            (sinkLazy)
import           Data.Digest.Pure.MD5                (MD5Digest, md5)
import           Data.List                           (nub, sortBy)
import           Data.Maybe                          (fromJust)
import           Data.Ord                            (comparing)
import           Data.Text                           (Text, pack, unpack)
import           Data.Text.Lazy                      (fromStrict)
import           Data.Text.Lazy.Encoding             (encodeUtf8)
import           Data.Time                           (UTCTime (..),
                                                      fromGregorian,
                                                      getCurrentTime)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries,
                                                      localTimeToUTC')
import           Network.AWS                         (send, sinkBody)
import           Network.AWS.Data                    (toText)
import           Network.AWS.Data.Body               (toBody)
import           Network.AWS.Easy                    (withAWS)
import           Network.AWS.S3                      (BucketName (..),
                                                      ObjectKey (..), getObject,
                                                      gorsBody, listObjects,
                                                      lorsContents, putObject)
import           Network.AWS.S3.Types                (oKey, oLastModified)
import           Network.AWS.SecretsManager          (getSecretValue,
                                                      gsvrsSecretString)
import           Network.Wreq                        (Options, defaults,
                                                      getWith, param,
                                                      responseBody)
import           Prelude                             (Bool (..), IO, Maybe (..),
                                                      Show (..), String, filter,
                                                      head, map, null, pure,
                                                      return, reverse,
                                                      undefined, ($), (++),
                                                      (/=), (==))
import           System.FilePath                     (FilePath, takeExtension)
import           System.Log.Logger                   (Priority (..), logL)
import           Types                               (APIKey (..), Env (..),
                                                      PartialPrice (..),
                                                      PartialPriceResponse (..),
                                                      Price (..),
                                                      PriceResponse (..),
                                                      S3Session (..),
                                                      SMSession (..),
                                                      SavedPrices (..))

convertPartialPrices :: TimeZoneSeries -> PartialPriceResponse -> PriceResponse
convertPartialPrices t pp =
  let lr  = localTimeToUTC' t (lastRefreshed (pp :: PartialPriceResponse))
      tz  = "UTC"
      sym = ticker (pp :: PartialPriceResponse)
      np  = map
        ( \p -> Price
          { priceTime = localTimeToUTC' t (partialTime p)
          , open      = open (p :: PartialPrice)
          , high      = high (p :: PartialPrice)
          , low       = low (p :: PartialPrice)
          , close     = close (p :: PartialPrice)
          , volume    = volume (p :: PartialPrice)
          , vwap      = vwap (p :: PartialPrice)
          }
        )
        (partialPrices pp)
  in  PriceResponse
        { lastRefreshed = lr
        , ticker        = sym
        , timeZone      = tz
        , prices        = np
        }


getLatestJSONFileRemote :: BucketName -> S3Session -> IO (Maybe FilePath)
getLatestJSONFileRemote b = withAWS $ do
  res <- send (listObjects b)
  let
    os =
      reverse
        $  sortBy (comparing (^. oLastModified))
        $  filter
             ( \o ->
               let k = unpack $ toText (o ^. oKey) in takeExtension k == ".json"
             )
        $  res
        ^. lorsContents
  case os of
    [] -> pure Nothing
    xs -> pure $ Just $ unpack (toText $ (head xs) ^. oKey)

readJSONFileRemote :: BucketName -> FilePath -> S3Session -> IO B.ByteString
readJSONFileRemote b f = withAWS $ do
  let k = ObjectKey (pack f)
  res <- send (getObject b k)
  let o = res ^. gorsBody
  sinkBody o sinkLazy

importLatestJSONFile
  :: (MonadReader Types.Env m, MonadIO m) => m (Maybe (IO B.ByteString))
importLatestJSONFile = do
  env    <- ask
  latest <- liftIO $ getLatestJSONFileRemote tslaqPricesBucket (s3Session env)
  case latest of
    Nothing -> pure Nothing
    Just (f) ->
      let bs = readJSONFileRemote tslaqPricesBucket f (s3Session env)
      in  pure $ Just (bs)

emptyPrices :: B.ByteString
emptyPrices = encodeUtf8
  ( "{\"lastRefreshed\":\"1970-01-01T00:00:00Z\",\"timeZone\":\"UTC\",\"ticker\":\"TSLA\",\"prices\":[{\"priceTime\":\"1970-01-01T00:00:00Z\",\"open\":0.00,\"high\":0.00,\"low\":0.00,\"close\":0.00,\"volume\":0.00,\"vwap\":null}]}"
  )

saved :: (MonadReader Types.Env m, MonadIO m) => m (SavedPrices)
saved = do
  l <- importLatestJSONFile
  case l of
    Nothing  -> return $ fromJust (decode emptyPrices :: Maybe SavedPrices)
    Just (d) -> do
      d' <- liftIO $ d
      return $ fromJust $ decode d'

updatePrices :: (MonadReader Types.Env m, MonadIO m) => m ()
updatePrices = do
  env <- ask
  let tzs' = tzs env
  savedPrices <- saved
  newPrices   <- downloadPrices
  currentTime <- liftIO $ getCurrentTime
  let new' = convertPartialPrices tzs' (fromJust newPrices)
  let combinedPrices =
        filter (\p -> (priceTime p) /= UTCTime (fromGregorian 1970 1 1) 0)
          $  nub
          $  (prices (savedPrices :: SavedPrices))
          ++ (prices (new' :: PriceResponse))
  let updatedPrices = SavedPrices currentTime "UTC" "TSLA" combinedPrices
  let u             = encode updatedPrices
  let h             = md5 u
  let localFilename = localPricesFolder ++ "prices-" ++ (show h) ++ ".json"
  liftIO $ B.writeFile localFilename u
  liftIO $ uploadToS3 tslaqPricesBucket u h (s3Session env)
  logMessage "OK"

-- csv to records
-- records to json
-- json to saved
-- run updatePrices
importAlgoSeek :: (MonadReader Types.Env m, MonadIO m) => m ()
importAlgoSeek = undefined

downloadOpts :: Text -> Bool -> Options
downloadOpts k getFull =
  defaults
    &  param "function"
    .~ ["TIME_SERIES_INTRADAY"]
    &  param "symbol"
    .~ ["TSLA"]
    &  param "interval"
    .~ ["60min"]
    &  param "outputsize"
    .~ [s]
    &  param "apikey"
    .~ [k]
  where s = if getFull then "full" else "compact"

logMessage :: (MonadReader Types.Env m, MonadIO m) => String -> m ()
logMessage msg = do
  env <- ask
  let l = envLog env
  liftIO $ logL l DEBUG msg


noSavedPrices :: BucketName -> S3Session -> IO Bool
noSavedPrices b = withAWS $ do
  res <- send (listObjects b)
  let os = res ^. lorsContents
  return (null os)

uploadToS3 :: BucketName -> B.ByteString -> MD5Digest -> S3Session -> IO ()
uploadToS3 b f h = withAWS $ do
  let k    = "prices-" ++ (show h) ++ ".json"
  let body = toBody f
  let k'   = ObjectKey (pack k)
  void $ send (putObject b k' body)

downloadPrices
  :: (MonadReader Types.Env m, MonadIO m) => m (Maybe PartialPriceResponse)
downloadPrices = do
  env     <- ask
  k       <- liftIO $ getApiKey "alphavantage" (secretsSession env)
  getFull <- liftIO $ noSavedPrices tslaqPricesBucket (s3Session env)
  res     <- liftIO $ getWith (downloadOpts k getFull) baseUrl
  pure $ decode (res ^. responseBody)

baseUrl :: String
baseUrl = "https://www.alphavantage.co/query"

tslaqPricesBucket :: BucketName
tslaqPricesBucket = "tslaq-prices"

localPricesFolder :: FilePath
localPricesFolder = "/var/local/tslaq-prices/"

getApiKey :: Text -> SMSession -> IO Text
getApiKey s = withAWS $ do
  res <- send (getSecretValue s)
  let k = res ^. gsvrsSecretString
  let k' =
        fromJust (decode (encodeUtf8 $ fromStrict $ fromJust k) :: Maybe APIKey)
  return (apiKey k')
