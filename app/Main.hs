module Main where

import HsData
import System.Environment (getArgs)

main :: IO ()
main = do
  values <- getArgs
  print $ map medianAndMean $ map fromInt $ map vowelIndices values

fromInt :: (Num b, Integral a) => [a] -> [b]
fromInt xs = map fromIntegral xs
