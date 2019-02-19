{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TSLAQPrices
  ( downloadPrices
  , updatePrices
  , saved
  , savePrices
  , getCurrentDate
  ) where

import Control.Lens

import Data.Aeson (decode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Text
import qualified Data.Text.Lazy.IO as I
import Network.Wreq
import Types
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Time (getCurrentTime, utctDay, Day)
import Control.Monad.IO.Class
import Control.Monad.Reader
import System.Log.Logger

downloadPrices :: IO (Maybe PriceResponse)
downloadPrices = do
  res <- getWith downloadOpts baseUrl
  return $ decode (res ^. responseBody)

baseUrl :: String
baseUrl = "https://www.alphavantage.co/query"

apiKey :: Text
apiKey = undefined

jsonFile :: String
jsonFile = "etc/tsla-daily-price-series.json"

downloadOpts :: Options
downloadOpts =
  defaults
    &  param "function"
    .~ ["TIME_SERIES_DAILY_ADJUSTED"]
    &  param "symbol"
    .~ ["TSLA"]
    &  param "outputsize"
    .~ ["compact"]
    &  param "apikey"
    .~ [apiKey]

importJSONFile :: (MonadReader Types.Env m, MonadIO m) => m B.ByteString
importJSONFile = do
  env <- ask
  liftIO $ B.readFile jsonFile

saved :: (MonadReader Types.Env m, MonadIO m) => m (Maybe SavedPrices)
saved = do
  env   <- ask
  daily <- importJSONFile
  return $ decode daily

savePrices :: IO ()
savePrices = do
  newPrices <- downloadPrices
  I.writeFile jsonFile (encodeToLazyText newPrices)

getCurrentDate :: (MonadReader Types.Env m, MonadIO m) => m Day
getCurrentDate = liftIO $ getCurrentTime >>= return . utctDay

updatePrices :: (MonadReader Types.Env m, MonadIO m) => m ()
updatePrices = do
  env         <- ask
  savedPrices <- saved
  newPrices   <- liftIO $ downloadPrices
  currentDate <- getCurrentDate
  let combinedPrices =
        nub
          $  (prices (fromJust $ savedPrices :: SavedPrices))
          ++ (prices (fromJust $ newPrices :: PriceResponse))
  let updatedPrices = SavedPrices currentDate "EST" combinedPrices
  liftIO $ I.writeFile
    ("etc/tsla-daily-price-series-" ++ (show currentDate) ++ ".json")
    (encodeToLazyText updatedPrices)
