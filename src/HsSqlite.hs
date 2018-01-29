module HsSqlite (convertCsvFileToSql) where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.CSV

convertCsvFileToSql :: String -> String -> String -> [String] -> IO ()
convertCsvFileToSql inFileName outFileName tableName fields = do
  input <- readFile inFileName
  let records = parseCSV inFileName input
  either
    handleCsvError
    convertTool records
  where
    convertTool = convertCsvToSql tableName outFileName fields
    handleCsvError csv = putStrLn "This is not a CSV file"

convertCsvToSql :: String -> FilePath -> [String] -> CSV -> IO ()
convertCsvToSql tableName outFileName fields records =
  if nFieldsInFile == nFieldsInFields then do
    conn <- connectSqlite3 outFileName
    run conn (createStatement tableName fields) []
    stmt <- prepare conn (insertStatement tableName nFieldsInFile)
    executeMany stmt
      (tail (filter (\record -> nFieldsInFile == length record) sqlRecords))
    commit conn
    disconnect conn
    putStrLn "Success"
  else
    putStrLn "The nr of input fields differ from the csv file"
  where
   nFieldsInFile = length $ head records
   nFieldsInFields = length fields
   sqlRecords = map (\record -> map (\element -> toSql element) record) records

insertStatement tableName nFieldsInFile =
  "INSERT INTO " ++ tableName ++
    " VALUES (" ++ (intercalate ", " (replicate nFieldsInFile "?")) ++ ")"

createStatement tableName fields =
  "CREATE TABLE " ++ tableName ++ " (" ++ (intercalate ", " fields) ++ ")"
