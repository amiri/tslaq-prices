{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AlgoSeek where

import           Data.Csv  (DefaultOrdered (..), FromNamedRecord (..),
                            ToField (..), ToNamedRecord (..), (.:))
import           Data.Time (LocalTime, defaultTimeLocale, formatTime,
                            parseTimeOrError)
import           Types     (PartialPrice (..))

-- Date,Ticker,TimeBarStart,FirstTradePrice,HighTradePrice,LowTradePrice,LastTradePrice,VolumeWeightPrice,Volume,TotalTrades
instance FromNamedRecord PartialPrice where
  parseNamedRecord m = do
    d       <- m .: "Date"
    t       <- m .: "TimeBarStart"
    open'   <- m .: "FirstTradePrice"
    high'   <- m .: "HighTradePrice"
    low'    <- m .: "LowTradePrice"
    close'  <- m .: "LastTradePrice"
    vwap'   <- m .: "VolumeWeightPrice"
    volume' <- m .: "Volume"
    let dt = parseTimeOrError True defaultTimeLocale "%Y%m%d%H:%M" (d ++ t) :: LocalTime
    return $ PartialPrice
      {
        partialTime = dt
      , open = read open'
      , high = read high'
      , low = read low'
      , close = read close'
      , volume = read volume'
      , partialVwap = Just (read vwap')
      }

instance ToNamedRecord PartialPrice
instance DefaultOrdered PartialPrice

instance ToField LocalTime where
      toField = toField . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
