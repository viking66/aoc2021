module AOC.Day01 (day01) where

import AOC.Types
import Safe (readMay)

day01 :: Solution [Int] Int Int
day01 =
    Solution
        { parse = traverse readMay . lines
        , part1 = Just . countIncreasing
        , part2 = Just . countIncreasing . tripleSums
        }

countIncreasing :: [Int] -> Int
countIncreasing xs = length . filter id . zipWith (<) xs $ drop 1 xs

tripleSums :: [Int] -> [Int]
tripleSums xs = zipWith3 (\x y z -> x + y + z) xs (drop 1 xs) (drop 2 xs)
