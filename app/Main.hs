module Main where

import HsData
import System.Environment (getArgs)

main :: IO ()
main = do
  values <- getArgs
  print $ map median $ map fromInt $ map vowelIndices values

fromInt xs = map fromIntegral xs
