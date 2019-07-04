{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AlgoSeek where

import           AWS                                 (saved, uploadPrices)
import           Codec.Archive.Zip
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask)
import qualified Data.ByteString.Lazy                as B (readFile)
import           Data.Csv                            (DefaultOrdered (..),
                                                      FromNamedRecord (..),
                                                      ToField (..),
                                                      ToNamedRecord (..), (.:))
import qualified Data.Csv                            as CSV
import           Data.List                           (groupBy, nub, sort)
import           Data.List.Split                     (splitOn)
import qualified Data.Map.Strict                     as M
import           Data.Time                           (LocalTime (..),
                                                      TimeOfDay (..),
                                                      UTCTime (..), addUTCTime,
                                                      defaultTimeLocale,
                                                      formatTime,
                                                      getCurrentTime,
                                                      parseTimeOrError,
                                                      timeOfDayToTime,
                                                      timeToTimeOfDay)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import qualified Data.Vector                         as V
import           System.Directory                    (createDirectoryIfMissing,
                                                      listDirectory)
import           Text.Regex.PCRE
import           Types                               (Env (..),
                                                      PartialPrice (..),
                                                      Price (..),
                                                      SavedPrices (..))
import           Util                                (combinePrices,
                                                      convertPartialPrice,
                                                      logMessage)

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

dataDir :: FilePath
dataDir = "algoseek-data"

getUnzipFolder :: EntrySelector -> FilePath
getUnzipFolder entry =
  "csv-data/" <> ((splitOn "." (unEntrySelector entry)) !! 0)

summarizeRecords :: [Price] -> Price
summarizeRecords ps =
  let p       = head ps
      l       = last ps
      ts      = timeStart (priceTime p)
      te      = addUTCTime (realToFrac (3600 :: Integer)) ts
      open'   = open (p :: Price)
      close'  = close (l :: Price)
      high'   = maximum $ map (\p1 -> high (p1 :: Price)) ps
      low'    = minimum $ map (\p2 -> low (p2 :: Price)) ps
      volume' = sum $ map (\p3 -> volume (p3 :: Price)) ps
      vwap'   = vwap (l :: Price)
  in  Price
        { priceTime = te
        , open      = open'
        , close     = close'
        , high      = high'
        , low       = low'
        , volume    = volume'
        , vwap      = vwap'
        }

belongsToHour :: Price -> Price -> Bool
belongsToHour p1 p2 =
  let start1 = timeStart (priceTime p1)
      start2 = timeStart (priceTime p2)
  in  start1 == start2

groupByHour :: [Price] -> [[Price]]
groupByHour = groupBy belongsToHour

timeStart :: UTCTime -> UTCTime
timeStart UTCTime { utctDay = tsd, utctDayTime = tst } =
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

parseCSV :: FilePath -> IO (V.Vector PartialPrice)
parseCSV f = do
  c <- B.readFile f
  case CSV.decodeByName c of
    Left  err -> fail err
    Right d   -> pure $ snd d

partialPriceVectorToPrices
  :: [[V.Vector PartialPrice]] -> TimeZoneSeries -> [Price]
partialPriceVectorToPrices v tzs =
  map (convertPartialPrice tzs) $ sort $ V.toList $ V.concat $ concat v

-- csv to records
-- records to json
-- combine with saved
-- upload as new
importAlgoSeek :: (MonadReader Types.Env m, MonadIO m) => m ()
importAlgoSeek = do
  env <- ask
  let tzs' = tzs env
  zipFiles   <- liftIO $ listDirectory dataDir
  tickerDirs <- mapM
    ( \zf -> do
      let p = dataDir <> "/" <> zf
      entries'   <- withArchive p (M.keys <$> getEntries)
      tickerDirs <- liftIO $ mapM
        ( \e -> do
          let unzipDir = getUnzipFolder e
          let filePath = ((splitOn "." zf) !! 0) <> ".csv"
          createDirectoryIfMissing True unzipDir
          withArchive p (saveEntry e (unzipDir <> "/" <> filePath))
          pure unzipDir
        )
        entries'
      pure tickerDirs
    )
    (sort zipFiles)
  let tickerDirs' = filter (=~ ("TSLA" :: String)) $ nub $ concat tickerDirs
  csvData <- liftIO $ mapM
    ( \dir -> do
      csvFiles <- listDirectory dir
      csvData' <- mapM (\f -> parseCSV (dir <> "/" <> f)) csvFiles
      pure csvData'
    )
    tickerDirs'
  let ps = map summarizeRecords $ groupByHour $ partialPriceVectorToPrices
        csvData
        tzs'
  savedPrices <- saved
  logMessage $ show savedPrices
  let combinedPrices = combinePrices (prices (savedPrices :: SavedPrices)) ps
  currentTime <- liftIO $ getCurrentTime
  let updatedPrices = SavedPrices currentTime "UTC" "TSLA" combinedPrices
  _ <- uploadPrices updatedPrices
  logMessage "importAlgoSeek OK"
