module HsData (medianAndMean, vowelIndices) where

import Data.List

medianAndMean xs = (median xs, avg xs)

avg :: (Real a, Fractional b) => [a] -> b
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

vowelIndices :: String -> [Integer]
vowelIndices word = map fst $ filter isVowel $ zip [1..] word

isVowel :: (Integer, Char) -> Bool
isVowel (_, letter) = elem letter "aeiouAEIOU"
