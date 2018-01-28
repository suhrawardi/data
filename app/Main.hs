module Main where

import HsData
import System.Environment (getArgs)

main :: IO ()
main = do
  values <- getArgs
  print $ map vowelIndices values
  -- print . median $ map read values
