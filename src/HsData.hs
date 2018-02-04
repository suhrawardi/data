module HsData (avg, median, medianAndMean, movingAverage, percentChange, readColumn) where

import Data.List

movingAverage :: [Double] -> Integer -> [Double]
movingAverage values window
  | window >= genericLength values = [avg values]
  | otherwise = avg (genericTake window values):(movingAverage (tail values) window)

percentChange :: Double -> Double -> Double
percentChange value first = 100.0 * (value - first) / first

medianAndMean xs = (median xs, avg xs)

avg :: (Real a, Fractional b) => [a] -> b
avg [] = 0
avg xs = realToFrac (sum xs) / fromIntegral (length xs)

median :: [Double] -> Double
median [] = 0
median xs = if oddInLength then
              middleValue
            else
              (middleValue + beforeMiddleValue) / 2
  where
    sortedList = sort xs
    oddInLength = 1 == mod (genericLength xs) 2
    middle = floor $ (genericLength xs) / 2
    middleValue = genericIndex sortedList middle
    beforeMiddleValue = genericIndex sortedList (middle - 1)

readColumn :: [String] -> [Double]
readColumn = map read
