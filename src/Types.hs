{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar (fromGregorian)
import Data.Traversable
import GHC.Generics
import Control.Monad.Trans.AWS (
    Env
  , Region(..)
  , Service
  )
import System.Log.Logger

-- import Data.Time.LocalTime.TimeZone.Olson
-- import Data.Time.LocalTime.TimeZone.Series
-- import Data.Fixed

data AWSInfo = AWSInfo {
    env :: Control.Monad.Trans.AWS.Env
  , region :: Region
  , s3Service :: Service
  , secretsService :: Service
  }

data Env = Env {
    envLog :: !Logger
  , envAWSInfo :: !AWSInfo
  }

data Price = Price
  { day :: Day
  , open :: Double
  , high :: Double
  , low :: Double
  , close :: Double
  } deriving (Show, Generic, ToJSON)

instance Ord Price where
  compare = comparing day

instance Eq Price where
  (Price d1 _ _ _ _) == (Price d2 _ _ _ _) = d1 == d2

instance FromJSON Price where
  parseJSON =
    withObject "Price" $ \obj -> do
      day <- obj .: "day"
      open <- obj .: "open"
      high <- obj .: "high"
      low <- obj .: "low"
      close <- obj .: "close"
      return Price {..}

data SavedPrices = SavedPrices
  { lastRefreshed :: Day
  , timeZone :: String
  , prices :: [Price]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PriceResponse = PriceResponse
  { lastRefreshed :: Day
  , timeZone :: String
  , prices :: [Price]
  } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON PriceResponse where
  parseJSON =
    withObject "PriceResponse" $ \obj -> do
      metaData <- obj .: "Meta Data"
      lastRefreshed' <- metaData .: "3. Last Refreshed"
      timeZone' <- metaData .: "5. Time Zone"
      prices' <- obj .: "Time Series (Daily)"
      prices'' <- parsePrices prices'
      -- let tzs = getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ timeZone')
      -- tzs' <- tzs
      -- let [day', _] = splitOn " " (T.unpack lastRefreshed')
      let day' = T.unpack lastRefreshed'
      let [y, m, d] = splitOn "-" day'
      -- let [h, min, s] = splitOn ":" time'
      let localTime' = fromGregorian (read y :: Integer) (read m :: Int) (read d :: Int)
              -- (TimeOfDay (read h :: Int) (read min :: Int) (read s))
      -- let utcTime' = localTimeToUTC' tzs' localTime'
      return
        PriceResponse
          { lastRefreshed = localTime'
          , timeZone = timeZone'
          , prices = sort prices''
          }

parsePrices :: Value -> Parser [Price]
parsePrices = withObject "prices" $ \o ->
  for (HM.toList o) $ \(day, Object priceData) -> do
    open'  <- priceData .: "1. open"
    high'  <- priceData .: "2. high"
    low'   <- priceData .: "3. low"
    close' <- priceData .: "4. close"
    let [year', month', day'] = splitOn "-" (T.unpack day)
    return $ Price
      { day   = fromGregorian (read year' :: Integer)
                              (read month' :: Int)
                              (read day' :: Int)
      , open  = read open'
      , high  = read high'
      , low   = read low'
      , close = read close'
      }
