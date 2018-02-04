module Main where

import Data.List.Split
import Database.HDBC
import Database.HDBC.Sqlite3
import Graphics.EasyPlot
import HsCsv
import HsData
import HsDownload
import HsPlot
import HsSqliteImport
import System.Environment (getArgs)
import System.FilePath.Posix
import Text.CSV

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

main :: IO ()
main = main03

main03 :: IO ()
main03 = do
  let baseName = "bitcoin"
      csvFile = "csv/" ++ baseName ++ ".csv"
      sqlFile = "tmp/" ++ baseName ++ ".sql"
  print $ "Using CSV file " ++ csvFile
  print $ "Using db " ++ sqlFile
  removeIfExists sqlFile
  convertCsvFileToSql csvFile sqlFile baseName
  resultRaw <- pullStockClosingPrices sqlFile baseName
  let result = take 365 resultRaw
      resultPc = applyPercentChange result
      resultMc50 = applyMovingAverage result 5
      resultMc200 = applyMovingAverage result 20
  plot (PNG (baseName ++ ".png")) $ [
    Data2D [Title baseName, Style Lines, Color Red] [] resultPc,
    Data2D [Title baseName, Style Lines, Color Black] [] resultMc50,
    Data2D [Title baseName, Style Lines, Color Yellow] [] resultMc200]
  return ()

main02 :: IO ()
main02 = do
  let url = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
      baseName = takeBaseName url
      csvFile = "tmp/" ++ baseName ++ ".csv"
      sqlFile = "tmp/" ++ baseName ++ ".sql"
  removeIfExists csvFile
  removeIfExists sqlFile
  downloadData url csvFile
  convertCsvFileToSql csvFile sqlFile baseName
  conn <- connectSqlite3 sqlFile
  latitudes <- quickQuery conn ("SELECT latitude FROM " ++ baseName) []
  let latitudesDbl = map (\record -> fromSql $ head record :: Double) latitudes
  print $ avg latitudesDbl

main01 :: IO ()
main01 = do
  values <- getArgs
  let url = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
      path = "tmp/" ++ takeFileName url
      columnName = head values
  downloadData url path
  res <- applyToColumnInCsvFile (avg . readColumn) path columnName
  print res

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
