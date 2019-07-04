{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Util

where

import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask)
import           Data.List                           (nub, sort)
import           Data.Time                           (UTCTime (..),
                                                      fromGregorian)
import           Data.Time.LocalTime.TimeZone.Olson  (getTimeZoneSeriesFromOlsonFile)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries,
                                                      localTimeToUTC')
import           System.Log.Logger                   (Priority (..), logL)
import           Types                               (Env (..),
                                                      PartialPrice (..),
                                                      PartialPriceResponse (..),
                                                      Price (..),
                                                      PriceResponse (..))


logMessage :: (MonadReader Types.Env m, MonadIO m) => String -> m ()
logMessage msg = do
  env <- ask
  let l = envLog env
  liftIO $ logL l DEBUG msg

convertPartialPrice :: TimeZoneSeries -> PartialPrice -> Price
convertPartialPrice t p =
  let pt = localTimeToUTC' t (partialTime p)
  in  Price
        { priceTime = pt
        , open      = open (p :: PartialPrice)
        , high      = high (p :: PartialPrice)
        , low       = low (p :: PartialPrice)
        , close     = close (p :: PartialPrice)
        , volume    = volume (p :: PartialPrice)
        , vwap      = partialVwap (p :: PartialPrice)
        }

convertPartialPriceResponse
  :: TimeZoneSeries -> PartialPriceResponse -> PriceResponse
convertPartialPriceResponse tzs pp =
  let lr  = localTimeToUTC' tzs (lastRefreshed (pp :: PartialPriceResponse))
      tz  = "UTC"
      sym = ticker (pp :: PartialPriceResponse)
      np  = map (convertPartialPrice tzs) (partialPrices pp)
  in  PriceResponse
        { lastRefreshed = lr
        , ticker        = sym
        , timeZone      = tz
        , prices        = np
        }

combinePrices :: [Price] -> [Price] -> [Price]
combinePrices xs ys =
  sort
    $  filter (\p -> (priceTime p) /= UTCTime (fromGregorian 1970 1 1) 0)
    $  nub
    $  xs
    ++ ys

getEasternTimeZoneSeries :: IO TimeZoneSeries
getEasternTimeZoneSeries =
  getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/US/Eastern")
