{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Types where

import           Data.Aeson                          (parseJSON, withObject,
                                                      (.:))
import           Data.Aeson.Types                    (FromJSON, Parser, ToJSON,
                                                      Value (..))
import qualified Data.HashMap.Strict                 as HM (toList)
import           Data.Int                            ()
import           Data.List                           (sort)
import           Data.List.Split                     (splitOn)
import           Data.Ord                            (Ord (..), comparing)
import qualified Data.Text                           as T (Text, unpack)
import           Data.Time                           (UTCTime)
import           Data.Time.Calendar                  (fromGregorian)
import           Data.Time.LocalTime                 (LocalTime (..),
                                                      TimeOfDay (..))
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import           Data.Traversable                    (for)
import           GHC.Generics                        (Generic)
import           Network.AWS.Easy                    (TypedSession,
                                                      wrapAWSService)
import           Network.AWS.S3                      (s3)
import           Network.AWS.SecretsManager          (secretsManager)
import           Prelude                             (Double, Eq (..), Int,
                                                      Integer, Maybe (..),
                                                      Show (..), pure, read,
                                                      return, ($))
import           System.Log.Logger                   (Logger)

wrapAWSService 's3 "S3Service" "S3Session"
wrapAWSService 'secretsManager "SMService" "SMSession"

data Env = Env {
    envLog         :: !Logger
  , s3Session      :: !(TypedSession S3Service)
  , secretsSession :: !(TypedSession SMService)
  , tzs            :: !(TimeZoneSeries)
  }

-- Date,Ticker,TimeBarStart,FirstTradePrice,HighTradePrice,LowTradePrice,LastTradePrice,VolumeWeightPrice,Volume,TotalTrades
data Price = Price
  { priceTime :: UTCTime
  , open      :: Double
  , high      :: Double
  , low       :: Double
  , close     :: Double
  , volume    :: Double
  , vwap      :: Maybe Double
  } deriving (Show, Generic, ToJSON)

data PartialPrice = PartialPrice
  { partialTime :: LocalTime
  , open        :: Double
  , high        :: Double
  , low         :: Double
  , close       :: Double
  , volume      :: Double
  , vwap        :: Maybe Double
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

instance Ord Price where
  compare = comparing priceTime

instance Ord PartialPrice where
  compare = comparing partialTime

instance Eq Price where
  (Price d1 _ _ _ _ _ _) == (Price d2 _ _ _ _ _ _) = d1 == d2

instance FromJSON Price where
  parseJSON =
    withObject "Price" $ \obj -> do
      priceTime <- obj .: "priceTime"
      open <- obj .: "open"
      high <- obj .: "high"
      low <- obj .: "low"
      close <- obj .: "close"
      volume <- obj .: "volume"
      vwap <- obj .: "vwap"
      return Price {..}


data APIKey = APIKey {
  apiKey :: T.Text
  } deriving (Show, Generic, ToJSON)

instance FromJSON APIKey where
  parseJSON =
    withObject "APIKey" $ \obj -> do
      apiKey <- obj .: "key"
      return APIKey {..}


data SavedPrices = SavedPrices
  { lastRefreshed :: UTCTime
  , timeZone      :: T.Text
  , ticker        :: T.Text
  , prices        :: [Price]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PriceResponse = PriceResponse
  { lastRefreshed :: UTCTime
  , timeZone      :: T.Text
  , ticker        :: T.Text
  , prices        :: [Price]
  } deriving (Eq, Show, Generic, ToJSON)

data PartialPriceResponse = PartialPriceResponse
  { lastRefreshed :: LocalTime
  , timeZone      :: T.Text
  , ticker        :: T.Text
  , partialPrices :: [PartialPrice]
  } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON PartialPriceResponse where
  parseJSON =
    withObject "PartialPriceResponse" $ \obj -> do
      metaData <- obj .: "Meta Data"
      sym <- metaData .: "2. Symbol"
      dt <- metaData .: "3. Last Refreshed"
      tz <- metaData .: "6. Time Zone"
      let localTime' = convertToLocalTime dt
      pPrices' <- obj .: "Time Series (60min)"
      pPrices'' <- parsePartialPrices pPrices'
      pure $
        PartialPriceResponse
          { lastRefreshed = localTime'
          , timeZone = tz
          , ticker = sym
          , partialPrices = sort pPrices''
          }

convertToLocalTime :: T.Text -> LocalTime
convertToLocalTime t =
  let [day', time'] = splitOn " " (T.unpack t)
      [y, m , d]    = splitOn "-" day'
      [h, m', s]    = splitOn ":" time'
  in  LocalTime
        (fromGregorian (read y :: Integer) (read m :: Int) (read d :: Int))
        (TimeOfDay (read h :: Int) (read m' :: Int) (read s))

parsePartialPrices :: Value -> Parser [PartialPrice]
parsePartialPrices =
  withObject "prices" $ \o -> for (HM.toList o) $ \(lt, Object priceData) -> do
    let localTime' = convertToLocalTime lt
    open'   <- priceData .: "1. open"
    high'   <- priceData .: "2. high"
    low'    <- priceData .: "3. low"
    close'  <- priceData .: "4. close"
    volume' <- priceData .: "5. volume"
    pure $ PartialPrice
      { partialTime = localTime'
      , open        = read open'
      , high        = read high'
      , low         = read low'
      , close       = read close'
      , volume      = read volume'
      , vwap        = Nothing
      }
