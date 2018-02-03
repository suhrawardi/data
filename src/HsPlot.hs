module HsPlot (pullStockClosingPrices) where

import Database.HDBC
import Database.HDBC.Sqlite3
import HsCsv
import HsSqliteImport
import HsSqliteQuery
import Text.CSV

pullStockClosingPrices :: String -> String -> IO [(Double, Double)]
pullStockClosingPrices sqlFile database = do
  conn <- connectSqlite3 sqlFile
  sqlResult  <- quickQuery conn ("SELECT rowid, adjclose FROM " ++ database) []
  return $ zip
    (reverse $ readDoubleColumn sqlResult 0)
    (readDoubleColumn sqlResult 1)
