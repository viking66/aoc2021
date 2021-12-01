module AOC.Day01 (day01) where

import AOC.Types
import Data.List (tails)
import Safe (readMay)

day01 :: Solution [Int] Int Int
day01 =
    Solution
        { parse = traverse readMay . lines
        , part1 = Just . countIncreasing
        , part2 = Just . countIncreasing . tripleSums
        }

countIncreasing :: [Int] -> Int
countIncreasing xs = length . filter (True ==) . zipWith (<) xs $ drop 1 xs

tripleSums :: [Int] -> [Int]
tripleSums = map (sum . take 3) . filter ((>= 3) . length) . tails
