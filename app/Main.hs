module Main where

import Data.List.Split
import Database.HDBC
import Database.HDBC.Sqlite3
import HsCsv
import HsData
import HsDownload
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
  let url = "https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1515012285&period2=1517690685&interval=1d&events=history&crumb=Bqk.lhpARaB"
      (baseName:_) = splitOn "?" $ takeBaseName url
      csvFile = "csv/" ++ baseName ++ ".csv"
      sqlFile = "tmp/" ++ baseName ++ ".sql"
  print $ "Using CSV file " ++ csvFile
  print $ "Using db " ++ sqlFile
  removeIfExists sqlFile
  convertCsvFileToSql csvFile sqlFile baseName
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
