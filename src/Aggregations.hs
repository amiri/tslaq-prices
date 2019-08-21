{-# LANGUAGE DuplicateRecordFields #-}

module Aggregations where

import           Data.List (groupBy)
import           Data.Time (LocalTime (..), TimeOfDay (..), UTCTime (..),
                            addUTCTime, timeOfDayToTime, timeToTimeOfDay)
import           Types     (PartialPrice (..), Price (..))

summarizeHourRecords :: [Price] -> Price
summarizeHourRecords ps =
  let p       = head ps
      l       = last ps
      ts      = hourStart (priceTime p)
      te      = addUTCTime (realToFrac (3600 :: Integer)) ts
      open'   = open (p :: Price)
      close'  = close (l :: Price)
      high'   = maximum $ map (\p1 -> high (p1 :: Price)) ps
      low'    = minimum $ map (\p2 -> low (p2 :: Price)) ps
      volume' = sum $ map (\p3 -> volume (p3 :: Price)) ps
      vwap'   = vwap (l :: Price)
  in  Price { priceTime = te
            , open      = open'
            , close     = close'
            , high      = high'
            , low       = low'
            , volume    = volume'
            , vwap      = vwap'
            }

summarizeDailyRecords :: [PartialPrice] -> PartialPrice
summarizeDailyRecords ps =
  let p       = head ps
      l       = last ps
      ts      = estDayStart (partialTime p)
      open'   = open (p :: PartialPrice)
      close'  = close (l :: PartialPrice)
      high'   = maximum $ map (\p1 -> high (p1 :: PartialPrice)) ps
      low'    = minimum $ map (\p2 -> low (p2 :: PartialPrice)) ps
      volume' = sum $ map (\p3 -> volume (p3 :: PartialPrice)) ps
      vwap'   = partialVwap (l :: PartialPrice)
  in  PartialPrice { partialTime = ts
            , open      = open'
            , close     = close'
            , high      = high'
            , low       = low'
            , volume    = volume'
            , partialVwap      = vwap'
            }

groupByHour :: [Price] -> [[Price]]
groupByHour = groupBy belongsToHour

groupByESTDay :: [PartialPrice] -> [[PartialPrice]]
groupByESTDay = groupBy belongsToESTDay

belongsToHour :: Price -> Price -> Bool
belongsToHour p1 p2 =
  let start1 = hourStart (priceTime p1)
      start2 = hourStart (priceTime p2)
  in  start1 == start2

belongsToESTDay :: PartialPrice -> PartialPrice -> Bool
belongsToESTDay p1 p2 =
  let start1 = estDayStart (partialTime p1)
      start2 = estDayStart (partialTime p2)
  in  start1 == start2

estDayStart :: LocalTime -> LocalTime
estDayStart LocalTime { localDay = ld } = LocalTime ld (TimeOfDay 0 0 0)

hourStart :: UTCTime -> UTCTime
hourStart UTCTime { utctDay = tsd, utctDayTime = tst } =
  let t = timeToTimeOfDay tst
      h = todHour t
      m = todMin t
      s = todSec t
      u = UTCTime tsd tst
      UTCTime { utctDay = utsd, utctDayTime = utst } =
          addUTCTime (realToFrac (-3600 :: Integer)) u
      ut = timeToTimeOfDay utst
      uh = todHour ut
  in  case (m < 30 && s <= 59) of
        True  -> UTCTime utsd (timeOfDayToTime (TimeOfDay uh 30 0))
        False -> UTCTime tsd (timeOfDayToTime (TimeOfDay h 30 0))

