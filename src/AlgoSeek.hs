{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AlgoSeek where

import           Aggregations                        (groupByHour,
                                                      summarizeHourRecords)
import           AWS                                 (saved, uploadPrices)
import           Codec.Archive.Zip
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, ask)
import qualified Data.ByteString.Lazy                as B (readFile)
import qualified Data.Csv                            as CSV
import           Data.List                           (nub, sort)
import           Data.List.Split                     (splitOn)
import qualified Data.Map.Strict                     as M
import           Data.Time                           (getCurrentTime)
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

dataDir :: FilePath
dataDir = "algoseek-data"

getUnzipFolder :: EntrySelector -> FilePath
getUnzipFolder entry =
  "csv-data/" <> ((splitOn "." (unEntrySelector entry)) !! 0)

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
    (\zf -> do
      let p = dataDir <> "/" <> zf
      entries'   <- withArchive p (M.keys <$> getEntries)
      tickerDirs <- liftIO $ mapM
        (\e -> do
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
    (\dir -> do
      csvFiles <- listDirectory dir
      csvData' <- mapM (\f -> parseCSV (dir <> "/" <> f)) csvFiles
      pure csvData'
    )
    tickerDirs'
  let ps = map summarizeHourRecords $ groupByHour $ partialPriceVectorToPrices
        csvData
        tzs'
  savedPrices <- saved
  logMessage $ show savedPrices
  let combinedPrices = combinePrices (prices (savedPrices :: SavedPrices)) ps
  currentTime <- liftIO $ getCurrentTime
  let updatedPrices = SavedPrices currentTime "UTC" "TSLA" combinedPrices
  _ <- uploadPrices updatedPrices
  logMessage "importAlgoSeek OK"
