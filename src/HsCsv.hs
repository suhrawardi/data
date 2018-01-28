module HsCsv (applyToColumnInCsvFile) where

import Data.Either
import Data.List
import Text.CSV

getColumnInCsvFile file columnName = do
  csv <- parseCSVFromFile file
  return $ either
    (\err -> Left "Problem reading file")
    (\csv -> getColumnInCsv csv columnName)
    csv

getColumnInCsv :: CSV -> String -> Either String Integer
getColumnInCsv csv columnName =
  case lookupResponse of
    Nothing -> Left "Column does not exist"
    Just x -> Right (fromIntegral x)
  where
  lookupResponse = findIndex (== columnName) (head csv)

applyToColumnInCsvFile :: ([String] -> b) -> FilePath -> String -> IO (Either String b)
applyToColumnInCsvFile func file column = do
    input <- readFile file
    let records = parseCSV file input
    return $ either
      handleCSVError
      (\csv -> applyToColumnInCsv func csv column)
      records
  where
    handleCSVError _ = Left "This does not appear to be a CSV file."

applyToColumnInCsv :: ([String] -> b) -> CSV -> String -> Either String b
applyToColumnInCsv func csv column = either
    (\_ -> Left "Error applying func")
    (Right . func . elements)
  columnIndex
  where
    columnIndex = getColumnInCsv csv column
    nFieldsInFile = length $ head csv
    records = tail $ filter (\record -> nFieldsInFile == length record) csv
    elements ci = map (\record -> genericIndex record ci) records
