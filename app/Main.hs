module Main where

import HsCsv
import HsData
import System.Environment (getArgs)

main :: IO ()
main = do
  values <- getArgs
  print $ head values
  res <- getColumnInCsvFile (head values) (last values)
  print res
  -- print $ map medianAndMean $ map (map fromIntegral) $ map vowelIndices values
