module AOC.Day07 (day07) where

import AOC.Types
import Data.List (find)
import Data.List.Split (splitOn)
import Safe (atMay, readMay)

day07 :: Solution [Int] Int Int
day07 =
    Solution
        { parse = traverse readMay . splitOn ","
        , part1 = stepCount id
        , part2 = stepCount (\n -> n * (n + 1) `div` 2)
        }

stepCount :: (Int -> Int) -> [Int] -> Maybe Int
stepCount f xs = snd . fst <$> (median >>= (find distLessThan . pairs . allStepCounts))
  where
    median = xs `atMay` (length xs `div` 2)
    dist n = sum $ map (f . abs . (n -)) xs
    allStepCounts b
        | da < db = iterate (go pred) (a, da)
        | dc < db = iterate (go succ) (c, dc)
        | otherwise = [(a, da), (b, db), (c, dc)]
      where
        (a, c) = (pred b, succ b)
        (da, db, dc) = (dist a, dist b, dist c)
        go f (n, d) = (f n, dist (f n))
    distLessThan ((_, dx), (_, dy)) = dx < dy
    pairs xs = zip xs (drop 1 xs)
