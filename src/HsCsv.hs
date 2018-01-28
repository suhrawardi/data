module HsCsv (getColumnInCsvFile) where

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
