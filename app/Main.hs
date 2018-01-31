module Main where

import Database.HDBC
import Database.HDBC.Sqlite3
import HsCsv
import HsData
import HsDownload
import HsSqlite
import System.Environment (getArgs)
import System.FilePath.Posix
import Text.CSV

main :: IO ()
main = main02

main02 :: IO ()
main02 = do
  values <- getArgs
  let url = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
      path = "tmp/" ++ takeFileName url
      columnName = head values
  downloadData url path
  convertCsvFileToSql path "earthquakes.sql" "earthquakes"
  res <- applyToColumnInCsvFile (minimum . readColumn) path columnName
  conn <- connectSqlite3 "earthquakes.sql"
  latitudes <- quickQuery conn "SELECT latitude FROM earthquakes" []
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
