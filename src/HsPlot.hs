module HsPlot (applyPercentChangeToData, pullStockClosingPrices) where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import HsCsv
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

percentChange :: Double -> Double -> Double
percentChange value first = 100.0 * (value - first) / first

applyPercentChangeToData :: [(Double, Double)] -> [(Double, Double)]
applyPercentChangeToData dataset = zip indices scaledData
  where
    (_, first) = last dataset
    indices = [1.0..(genericLength dataset)]
    scaledData = map
      (\(_, value) -> percentChange value first)
      dataset
