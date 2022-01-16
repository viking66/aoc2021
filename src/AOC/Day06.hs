module AOC.Day06 (day06) where

import AOC.Types
import Data.List.Split (splitOn)
import qualified Data.MultiSet as MultiSet
import Safe (atMay, readMay)

day06 :: Solution Fish Int Int
day06 =
    Solution
        { parse = fmap toFish . traverse readMay . splitOn ","
        , part1 = sumFish 80
        , part2 = sumFish 256
        }

data Fish = Fish Int Int Int Int Int Int Int Int Int
    deriving (Show)

toFish :: [Int] -> Fish
toFish = go . MultiSet.fromList
  where
    go s = Fish (cnt 0) (cnt 1) (cnt 2) (cnt 3) (cnt 4) (cnt 5) (cnt 6) (cnt 7) (cnt 8)
      where
        cnt n = MultiSet.occur n s

update :: Fish -> Fish
update (Fish a b c d e f g h i) = Fish b c d e f g (h + a) i a

sumFish :: Int -> Fish -> Maybe Int
sumFish day = fmap sum' . (`atMay` day) . iterate update
  where
    sum' (Fish a b c d e f g h i) = sum [a, b, c, d, e, f, g, h, i]
