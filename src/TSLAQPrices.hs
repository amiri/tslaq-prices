{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TSLAQPrices
  ( updatePrices
  , getLatestJSONFile
  , localPricesFolder
  , saved
  , emptyPrices
  ) where

import           Control.Lens

import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask)
import           Data.Aeson                          (decode, encode)
import qualified Data.ByteString.Lazy                as B (ByteString, readFile,
                                                           writeFile)
import           Data.Digest.Pure.MD5                (MD5Digest (..), md5)
import           Data.List                           (nub, sortBy)
import           Data.Maybe                          (fromJust)
import           Data.Ord                            (comparing)
import           Data.Text                           (Text)
import           Data.Text.Lazy                      (fromStrict)
import           Data.Text.Lazy.Encoding             (encodeUtf8)
import           Data.Time                           (UTCTime (..),
                                                      defaultTimeLocale,
                                                      formatTime, fromGregorian,
                                                      getCurrentTime)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries,
                                                      localTimeToUTC')
import           Network.AWS                         (send)
import           Network.AWS.Data.Body               (toBody)
import           Network.AWS.Easy                    (withAWS)
import           Network.AWS.S3                      (BucketName (..),
                                                      ObjectKey (..),
                                                      listObjects, lorsContents,
                                                      putObject)
import           Network.AWS.SecretsManager          (getSecretValue,
                                                      gsvrsSecretString)
import           Network.Wreq                        (Options, defaults,
                                                      getWith, param,
                                                      responseBody)
import           System.Directory                    (getModificationTime,
                                                      listDirectory)
import           System.FilePath                     (takeExtension)
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
convertPartialPrices tzs pp =
  let lr  = localTimeToUTC' tzs (lastRefreshed (pp :: PartialPriceResponse))
      tz  = "UTC"
      sym = ticker (pp :: PartialPriceResponse)
      np  = map
        ( \p -> Price
          { priceTime = localTimeToUTC' tzs (partialTime p)
          , open      = open (p :: PartialPrice)
          , high      = high (p :: PartialPrice)
          , low       = low (p :: PartialPrice)
          , close     = close (p :: PartialPrice)
          , volume    = volume (p :: PartialPrice)
          }
        )
        (partialPrices pp)
  in  PriceResponse
        { lastRefreshed = lr
        , ticker        = sym
        , timeZone      = tz
        , prices        = np
        }

getLatestJSONFile :: IO (Maybe FilePath)
getLatestJSONFile = do
  fs <- listDirectory localPricesFolder
  let fs' = filter (\f -> takeExtension f == ".json") fs
  fms <- mapM (\f -> getFormattedTime (localPricesFolder ++ f)) fs'
  let fms' = sortBy (comparing snd) $ zip fs' fms
  return $ case null fms' of
    True  -> Nothing
    False -> Just (fst $ last fms')


readJSONFile :: Maybe FilePath -> Maybe (IO B.ByteString)
readJSONFile (Just f) = Just (B.readFile f)
readJSONFile _        = Nothing


importLatestJSONFile
  :: (MonadReader Types.Env m, MonadIO m) => m (Maybe (IO B.ByteString))
importLatestJSONFile = do
  env    <- ask
  latest <- liftIO $ getLatestJSONFile
  case latest of
    Nothing -> return Nothing
    Just f  -> return $ readJSONFile $ Just (localPricesFolder ++ "/" ++ f)

emptyPrices :: B.ByteString
emptyPrices = encodeUtf8
  ( "{\"lastRefreshed\":\"1970-01-01T00:00:00Z\",\"timeZone\":\"UTC\",\"prices\":[{\"priceTime\":\"1970-01-01T00:00:00Z\",\"open\":0.00,\"high\":0.00,\"low\":0.00,\"close\":0.00,\"volume\":0.00}]}"
  )

saved :: (MonadReader Types.Env m, MonadIO m) => m (SavedPrices)
saved = do
  daily <- importLatestJSONFile
  case daily of
    Nothing  -> return $ fromJust (decode emptyPrices :: Maybe SavedPrices)
    Just (d) -> do
      d' <- liftIO d
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
  let updatedPrices = SavedPrices currentTime "UTC" combinedPrices
  liftIO $ print updatedPrices
  let u             = encode updatedPrices
  let h             = md5 u
  let localFilename = localPricesFolder ++ "prices-" ++ (show h) ++ ".json"
  liftIO $ B.writeFile localFilename u
  liftIO $ uploadToS3 tslaqPricesBucket u h (s3Session env)
  logMessage "OK"

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
  void $ send (putObject b (ObjectKey (read (show k) :: Text)) body)

downloadPrices
  :: (MonadReader Types.Env m, MonadIO m) => m (Maybe PartialPriceResponse)
downloadPrices = do
  env     <- ask
  k       <- liftIO $ getApiKey "alphavantage" (secretsSession env)
  getFull <- liftIO $ noSavedPrices tslaqPricesBucket (s3Session env)
  res     <- liftIO $ getWith (downloadOpts k getFull) baseUrl
  liftIO $ print res
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

getFormattedTime :: FilePath -> IO String
getFormattedTime f = do
  mt <- getModificationTime f
  let mt' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S %Z" mt
  return mt'
