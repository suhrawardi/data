module Main where

import HsCsv
import HsData
import HsDownload
import System.Environment (getArgs)
import System.FilePath.Posix
import Text.CSV

main :: IO ()
main = do
  values <- getArgs
  let url = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
      path = "tmp/" ++ takeFileName url
      columnName = head values
  downloadData url path
  res <- applyToColumnInCsvFile (minimum . readColumn) path columnName
  print res
