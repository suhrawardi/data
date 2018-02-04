module HsPlot (applyMovingAverage, applyPercentChange, pullStockClosingPrices) where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import HsCsv
import HsData
import HsSqliteImport
import HsSqliteQuery
import Text.CSV

pullStockClosingPrices :: String -> String -> IO [(Double, Double)]
pullStockClosingPrices sqlFile database = do
  conn <- connectSqlite3 sqlFile
  sqlResult  <- quickQuery conn ("SELECT rowid, price FROM " ++ database) []
  return $ zip
    (readDoubleColumn sqlResult 0)
    (readDoubleColumn sqlResult 1)

applyMovingAverage :: [(Double, Double)] -> Integer -> [(Double, Double)]
applyMovingAverage dataset window =
  zip [fromIntegral window..] $ movingAverage (map snd (dataset)) window

applyPercentChange :: [(Double, Double)] -> [(Double, Double)]
applyPercentChange dataset = zip indices scaledData
  where
    (_, first) = last dataset
    indices = [1.0..(genericLength dataset)]
    scaledData = map
      (\(_, value) -> percentChange value first)
      dataset
