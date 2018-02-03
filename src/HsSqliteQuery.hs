module HsSqliteQuery where

import Data.List
import Database.HDBC.Sqlite3
import Database.HDBC
import Graphics.EasyPlot
import HsSqliteImport

readIntegerColumn :: [[SqlValue]] -> Integer -> [Integer]
readIntegerColumn sqlResult index = map (\row -> fromSql $
  genericIndex row index :: Integer) sqlResult

readDoubleColumn :: [[SqlValue]] -> Integer -> [Double]
readDoubleColumn sqlResult index = map (\row -> fromSql $
  genericIndex row index :: Double) sqlResult

readStringColumn :: [[SqlValue]] -> Integer -> [String]
readStringColumn sqlResult index = map (\row -> fromSql $
  genericIndex row index :: String) sqlResult

queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile sqlQuery = do
  conn <- connectSqlite3 databaseFile
  let result = quickQuery' conn sqlQuery []
  disconnect conn
  result
