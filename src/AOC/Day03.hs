{-# LANGUAGE LambdaCase #-}

module AOC.Day03 (day03) where

import AOC.Types
import Control.Applicative (liftA2)
import Data.List (foldl', sort, transpose, uncons)

day03 :: Solution [[Bit]] Int Int
day03 =
    Solution
        { parse = traverse parseLine . lines
        , part1 = compute gammaRate epsilonRate
        , part2 = compute oxygenGeneratorRating co2ScrubberRating
        }

data Bit = Z | O
    deriving (Show, Eq)

toInt :: [Bit] -> Int
toInt = foldl' (\n b -> n * 2 + toDigit b) 0
  where
    toDigit = \case
        Z -> 0
        O -> 1

parseLine :: String -> Maybe [Bit]
parseLine = traverse toBit
  where
    toBit = \case
        '0' -> Just Z
        '1' -> Just O
        _ -> Nothing

data BitCounts = BitCounts Int Int

bitCounts :: [Bit] -> BitCounts
bitCounts bs = BitCounts (bitCount Z) (bitCount O)
  where
    bitCount b = length $ filter (b ==) bs

processReport :: (BitCounts -> Bit) -> (Bit -> Bit -> Bool) -> [[Bit]] -> Maybe Int
processReport computeBit numFilter = go []
  where
    go :: [Bit] -> [[Bit]] -> Maybe Int
    go bs bss
        | length bss <= 1 = Just . toInt $ reverse bs <> concat bss
        | otherwise = do
            parts <- traverse uncons bss
            let b = computeBit . bitCounts $ map fst parts
                bss' = filter (not . null) . map snd $ filter (numFilter b . fst) parts
            go (b:bs) bss'

noFilter :: Bit -> Bit -> Bool
noFilter _ _ = True

gammaRate :: [[Bit]] -> Maybe Int
gammaRate = processReport computeBit noFilter
  where
    computeBit (BitCounts z o) = if z >= o then Z else O

epsilonRate :: [[Bit]] -> Maybe Int
epsilonRate = processReport computeBit noFilter
  where
    computeBit (BitCounts z o) = if z <= o then Z else O

oxygenGeneratorRating :: [[Bit]] -> Maybe Int
oxygenGeneratorRating = processReport computeBit (==)
  where
    computeBit (BitCounts z o) = if z > o then Z else O

co2ScrubberRating :: [[Bit]] -> Maybe Int
co2ScrubberRating = processReport computeBit (==)
  where
    computeBit (BitCounts z o) = if z <= o then Z else O

compute :: ([[Bit]] -> Maybe Int) -> ([[Bit]] -> Maybe Int) -> [[Bit]] -> Maybe Int
compute f g bss = liftA2 (*) (f bss) (g bss)
