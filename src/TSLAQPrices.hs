{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TSLAQPrices
  ( downloadPrices
  , updatePrices
  , saved
  , getCurrentDate
  , getApiKey
  , importLatestJSONFile
  , getLatestJSONFile
  , getFormattedTime
  , readJSONFile
  , emptyPrices
  ) where

import           Control.Lens

import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Aeson                 (decode, encode)
import qualified Data.ByteString.Lazy       as B (ByteString, readFile,
                                                  writeFile)
import           Data.Digest.Pure.MD5       (MD5Digest (..), md5)
import           Data.List                  (nub, sortBy)
import           Data.Maybe                 (fromJust)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import           Data.Text.Lazy             (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Data.Time                  (Day, defaultTimeLocale, formatTime,
                                             fromGregorian, getCurrentTime,
                                             utctDay)
import           Network.AWS                (send)
import           Network.AWS.Data.Body      (toBody)
import           Network.AWS.Easy           (withAWS)
import           Network.AWS.S3             (BucketName (..), ObjectKey (..),
                                             listObjects, lorsContents,
                                             putObject)
import           Network.AWS.SecretsManager (getSecretValue, gsvrsSecretString)
import           Network.Wreq               (Options, defaults, getWith, param,
                                             responseBody)
import           System.Directory           (getModificationTime, listDirectory)
import           System.FilePath            (takeExtension)
import           System.Log.Logger          (Priority (..), logL)
import           Types                      (APIKey (..), Env (..), Price (..),
                                             PriceResponse (..), S3Session (..),
                                             SMSession (..), SavedPrices (..))


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
  ( "{\"lastRefreshed\":\"1970-01-01\", \"timeZone\": \"EST\", \"prices\":[{\"low\":0.0,\"close\":0.0,\"open\":0.0,\"day\":\"1970-01-01\",\"high\":0.0}]}"
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
  env         <- ask
  savedPrices <- saved
  newPrices <- downloadPrices
  currentDate <- getCurrentDate
  let combinedPrices =
        filter (\p -> (day p) /= fromGregorian 1970 1 1)
          $  nub
          $  (prices (savedPrices :: SavedPrices))
          ++ (prices (fromJust $ newPrices :: PriceResponse))
  let updatedPrices = SavedPrices currentDate "EST" combinedPrices
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
    .~ ["TIME_SERIES_DAILY_ADJUSTED"]
    &  param "symbol"
    .~ ["TSLA"]
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
  :: (MonadReader Types.Env m, MonadIO m) => m (Maybe PriceResponse)
downloadPrices = do
  env     <- ask
  k       <- liftIO $ getApiKey "alphavantage" (secretsSession env)
  getFull <- liftIO $ noSavedPrices tslaqPricesBucket (s3Session env)
  res     <- liftIO $ getWith (downloadOpts k getFull) baseUrl
  return $ decode (res ^. responseBody)

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

getCurrentDate :: (MonadReader Types.Env m, MonadIO m) => m Day
getCurrentDate = liftIO $ getCurrentTime >>= return . utctDay
