module Tests.AlgoSeek (tests)
  where

import           AlgoSeek
import           Data.Time
import           Data.Time                           (UTCTime (..),
                                                      fromGregorian)
import           Data.Time.Clock                     (secondsToDiffTime)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import qualified Data.Vector                         as V
import           System.IO.Unsafe                    (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.Hspec                    as Hspec
import           Types                               (PartialPrice, Price (..))
import           Util                                (getEasternTimeZoneSeries)
import Aggregations (hourStart, groupByHour, summarizeHourRecords)

tests :: TestTree
tests = testGroup
  "AlgoSeek"
  [ unsafePerformIO (testSpec "hourStart" spec_hourStart)
  , unsafePerformIO (testSpec "summarizeHourRecords" spec_summarizeHourRecords)
  , unsafePerformIO (testSpec "groupByHour" spec_groupByHour)
  ]

spec_hourStart :: SpecWith ()
spec_hourStart = do
  let d    = fromGregorian 2019 4 18
      tod1 = TimeOfDay 5 29 59
      tod2 = TimeOfDay 5 30 0
      tod3 = TimeOfDay 5 30 1
      t1   = UTCTime d (timeOfDayToTime tod1)
      t2   = UTCTime d (timeOfDayToTime tod2)
      t3   = UTCTime d (timeOfDayToTime tod3)
      et1  = UTCTime d (timeOfDayToTime (TimeOfDay 4 30 0))
      et2  = UTCTime d (timeOfDayToTime (TimeOfDay 5 30 0))
  it "hour:29:59 hourStart is (hour-1):30:00" $ do
    hourStart t1 `shouldBe` et1
  it "hour:30:00 hourStart is hour:30:00" $ do
    hourStart t2 `shouldBe` et2
  it "hour:30:01 hourStart is hour:30:00" $ do
    hourStart t3 `shouldBe` et2

withCsvAndTimeZoneSeries :: IO (V.Vector PartialPrice, TimeZoneSeries)
withCsvAndTimeZoneSeries = do
  tzs <- getEasternTimeZoneSeries
  csv <- parseCSV "tests/data/20190418.csv"
  pure (csv, tzs)

spec_summarizeHourRecords :: SpecWith ()
spec_summarizeHourRecords = do
  Hspec.before withCsvAndTimeZoneSeries $ do
    it "summarizes correctly" $ \(csv, tzs) -> do
      let
        ps      = partialPriceVectorToPrices [[csv]] tzs
        grouped = groupByHour ps
        pps     = map summarizeHourRecords grouped
        expected =
          [ Price
            { priceTime = UTCTime
              (fromGregorian 2019 4 18)
              (secondsToDiffTime ((8 * 60 * 60) + (30 * 60)))
            , open      = 271.03
            , high      = 271.03
            , low       = 268.73
            , close     = 269.12
            , volume    = 2304
            , vwap      = Just 269.12
            }
          , Price
            { priceTime = UTCTime
              (fromGregorian 2019 4 18)
              (secondsToDiffTime ((9 * 60 * 60) + (30 * 60)))
            , open      = 268.76
            , high      = 269.53
            , low       = 268.72
            , close     = 269.53
            , volume    = 2488
            , vwap      = Just 269.51636
            }
          , Price
            { priceTime = UTCTime
              (fromGregorian 2019 4 18)
              (secondsToDiffTime ((10 * 60 * 60) + (30 * 60)))
            , open      = 269.87
            , high      = 271.1
            , low       = 269.51
            , close     = 270.35
            , volume    = 8230
            , vwap      = Just 270.37648
            }
          , Price
            { priceTime = UTCTime
              (fromGregorian 2019 4 18)
              (secondsToDiffTime ((11 * 60 * 60) + (30 * 60)))
            , open      = 270.45
            , high      = 270.52
            , low       = 270.4
            , close     = 270.5
            , volume    = 2038
            , vwap      = Just 270.49627
            }
          ]
      pps `shouldBe` expected


spec_groupByHour :: SpecWith ()
spec_groupByHour = do
  Hspec.before withCsvAndTimeZoneSeries $ do
    it "groups correctly" $ \(csv, tzs) -> do
      let ps      = partialPriceVectorToPrices [[csv]] tzs
          grouped = groupByHour ps
      length (grouped !! 0) `shouldBe` 14
      length (grouped !! 1) `shouldBe` 13
      length (grouped !! 2) `shouldBe` 18
      length (grouped !! 3) `shouldBe` 1
