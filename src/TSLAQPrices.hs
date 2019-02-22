{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TSLAQPrices
  ( downloadPrices
  , updatePrices
  , saved
  , savePrices
  , getCurrentDate
  , getApiKey
  ) where

import           Control.Lens

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask)
import           Data.Aeson                 (decode)
import           Data.Aeson.Text            (encodeToLazyText)
import qualified Data.ByteString.Lazy       as B (ByteString, readFile)
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Data.Text.Lazy             (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import qualified Data.Text.Lazy.IO          as I (writeFile)
import           Data.Time                  (Day, getCurrentTime, utctDay)
import           Network.AWS                (send)
import           Network.AWS.Easy           (withAWS)
import           Network.AWS.S3             ()
import           Network.AWS.SecretsManager (getSecretValue, gsvrsSecretString)
import           Network.Wreq               (Options, defaults, getWith, param,
                                             responseBody)
import           System.Log.Logger          (Priority (..), logM)
import           Types                      (APIKey (..), Env (..),
                                             PriceResponse (..), SMSession (..),
                                             SavedPrices (..))

logMessage :: (MonadReader Types.Env m, MonadIO m) => String -> m ()
logMessage msg = do
  env <- ask
  liftIO $ logM "main" DEBUG msg

downloadPrices
  :: (MonadReader Types.Env m, MonadIO m) => m (Maybe PriceResponse)
downloadPrices = do
  env <- ask
  k   <- liftIO $ getApiKey "alphavantage" (secretsSession env)
  res <- liftIO $ getWith (downloadOpts k) baseUrl
  return $ decode (res ^. responseBody)

baseUrl :: String
baseUrl = "https://www.alphavantage.co/query"

getApiKey :: Text -> SMSession -> IO Text
getApiKey s = withAWS $ do
  res <- send (getSecretValue s)
  let k = res ^. gsvrsSecretString
  let k' =
        fromJust (decode (encodeUtf8 $ fromStrict $ fromJust k) :: Maybe APIKey)
  return (apiKey k')

jsonFile :: String
jsonFile = "etc/tsla-daily-price-series.json"

downloadOpts :: Text -> Options
downloadOpts k =
  defaults
    &  param "function"
    .~ ["TIME_SERIES_DAILY_ADJUSTED"]
    &  param "symbol"
    .~ ["TSLA"]
    &  param "outputsize"
    .~ ["compact"]
    &  param "apikey"
    .~ [k]

importJSONFile :: (MonadReader Types.Env m, MonadIO m) => m B.ByteString
importJSONFile = do
  env <- ask
  liftIO $ B.readFile jsonFile

saved :: (MonadReader Types.Env m, MonadIO m) => m (Maybe SavedPrices)
saved = do
  env   <- ask
  daily <- importJSONFile
  return $ decode daily

savePrices :: (MonadReader Types.Env m, MonadIO m) => m ()
savePrices = do
  newPrices <- downloadPrices
  liftIO $ I.writeFile jsonFile (encodeToLazyText newPrices)

getCurrentDate :: (MonadReader Types.Env m, MonadIO m) => m Day
getCurrentDate = liftIO $ getCurrentTime >>= return . utctDay

updatePrices :: (MonadReader Types.Env m, MonadIO m) => m ()
updatePrices = do
  env         <- ask
  savedPrices <- saved
  newPrices   <- downloadPrices
  currentDate <- getCurrentDate
  logMessage "Inside updatePrices"
  let combinedPrices =
        nub
          $  (prices (fromJust $ savedPrices :: SavedPrices))
          ++ (prices (fromJust $ newPrices :: PriceResponse))
  logMessage "Got combinedPrices"
  let updatedPrices = SavedPrices currentDate "EST" combinedPrices
  liftIO $ I.writeFile
    ("etc/tsla-daily-price-series-" ++ (show currentDate) ++ ".json")
    (encodeToLazyText updatedPrices)
