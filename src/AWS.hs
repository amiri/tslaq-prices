{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module AWS where

import           Control.Lens
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Aeson                 (decode, encode)
import qualified Data.ByteString.Lazy       as B (ByteString)
import           Data.Conduit.Combinators   (sinkLazy)
import           Data.Digest.Pure.MD5       (MD5Digest, md5)
import           Data.List                  (sortBy)
import           Data.Maybe                 (fromJust)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text, pack, unpack)
import           Data.Text.Lazy             (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Network.AWS                (send, sinkBody)
import           Network.AWS.Data           (toText)
import           Network.AWS.Data.Body      (toBody)
import           Network.AWS.Easy           (withAWS)
import           Network.AWS.S3             (BucketName (..), ObjectKey (..),
                                             getObject, gorsBody, listObjects,
                                             lorsContents, putObject)
import           Network.AWS.S3.Types       (oKey, oLastModified)
import           Network.AWS.SecretsManager (getSecretValue, gsvrsSecretString)
import           System.FilePath            (FilePath, takeExtension)
import           Text.Regex.PCRE
import           Types                      (APIKey (..), Env (..),
                                             HourlyAndDailyPrices (..),
                                             S3Session (..), SMSession (..),
                                             SavedPrices (..))
import           Util                       (logMessage)

tslaqPricesBucket :: BucketName
tslaqPricesBucket = "tslaq-prices"

emptyPrices :: B.ByteString
emptyPrices = encodeUtf8
  ("{\"lastRefreshed\":\"1970-01-01T00:00:00Z\",\"timeZone\":\"UTC\",\"ticker\":\"TSLA\",\"prices\":[{\"priceTime\":\"1970-01-01T00:00:00Z\",\"open\":0.00,\"high\":0.00,\"low\":0.00,\"close\":0.00,\"volume\":0,\"vwap\":null}]}"
  )

getApiKey :: Text -> SMSession -> IO Text
getApiKey s = withAWS $ do
  res <- send (getSecretValue s)
  let k = res ^. gsvrsSecretString
  let k' =
        fromJust (decode (encodeUtf8 $ fromStrict $ fromJust k) :: Maybe APIKey)
  return (apiKey k')

getLatestJSONFileRemote :: BucketName -> S3Session -> IO (Maybe FilePath)
getLatestJSONFileRemote b = withAWS $ do
  res <- send (listObjects b)
  let
    os =
      reverse
        $  sortBy (comparing (^. oLastModified))
        $  filter
             (\o ->
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

saved :: (MonadReader Types.Env m, MonadIO m) => m (SavedPrices)
saved = do
  l <- importLatestJSONFile
  case l of
    Nothing  -> return $ fromJust (decode emptyPrices :: Maybe SavedPrices)
    Just (d) -> do
      d' <- liftIO $ d
      if (d' =~ ("combined" :: B.ByteString) :: Bool)
        then pure
          (hourly (fromJust $ (decode d' :: Maybe HourlyAndDailyPrices)))
        else pure (fromJust $ decode d')

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

uploadPrices
  :: (MonadReader Types.Env m, MonadIO m) => HourlyAndDailyPrices -> m ()
uploadPrices ps = do
  env <- ask
  let u = encode ps
  let h = md5 u
  liftIO $ uploadToS3 tslaqPricesBucket u h (s3Session env)
  -- logMessage "uploadPrices OK"
