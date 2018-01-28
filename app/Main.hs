module Main where

import HsCsv
import HsData
import HsDownload
import System.Environment (getArgs)
import System.FilePath.Posix

main :: IO ()
main = do
  values <- getArgs
  print $ values
  let url = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv"
      path = "tmp/" ++ takeFileName url
  resp <- downloadData url path
  res <- getColumnInCsvFile path (head values)
  print res
  print $ map medianAndMean $ map (map fromIntegral) $ map vowelIndices values
