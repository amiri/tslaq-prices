{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module TSLAQPrices
  ( updatePrices
  ) where

import           AlgoSeek               ()
import           AWS
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Aeson             (decode)
import qualified Data.ByteString.Char8  as C8 ()
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import           Data.Time              (getCurrentTime)
import           Network.Wreq           (Options, defaults, getWith, param,
                                         responseBody)
import           Types                  (Env (..), PartialPriceResponse (..),
                                         PriceResponse (..), SavedPrices (..))
import           Util                   (combinePrices,
                                         convertPartialPriceResponse,
                                         logMessage)

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

updatePrices :: (MonadReader Types.Env m, MonadIO m) => m ()
updatePrices = do
  env <- ask
  let tzs' = tzs env
  savedPrices <- saved
  newPrices   <- downloadPrices
  let new' = convertPartialPriceResponse tzs' (fromJust newPrices)
  let combinedPrices = combinePrices (prices (savedPrices :: SavedPrices))
                                     (prices (new' :: PriceResponse))
  currentTime <- liftIO $ getCurrentTime
  let updatedPrices = SavedPrices currentTime "UTC" "TSLA" combinedPrices
  _ <- uploadPrices updatedPrices
  logMessage "updatePrices OK"
