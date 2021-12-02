{-# LANGUAGE LambdaCase #-}

module AOC.Day02 (day02) where

import AOC.Types
import Data.List (foldl')
import Safe (readMay)

day02 :: Solution [Command] Int Int
day02 =
    Solution
        { parse = traverse parseCommand . lines
        , part1 = Just . runCommands runCommandWithoutAim (0, 0, 0)
        , part2 = Just . runCommands runCommandWithAim (0, 0, 0)
        }

data Command = Forward Int | Down Int | Up Int
    deriving (Show)

parseCommand :: String -> Maybe Command
parseCommand = go . words
  where
    go [dir, dist] = parseDirection dir <*> readMay dist
    go _ = Nothing
    parseDirection = \case
        "forward" -> Just Forward
        "down" -> Just Down
        "up" -> Just Up
        _ -> Nothing

runCommands :: ((Int, Int, Int) -> Command -> (Int, Int, Int)) -> (Int, Int, Int) -> [Command] -> Int
runCommands f loc = uncurry (*) . getPos . foldl' f loc
  where
    getPos (hor, dep, _) = (hor, dep)

runCommandWithoutAim :: (Int, Int, Int) -> Command -> (Int, Int, Int)
runCommandWithoutAim (hor, dep, aim) = \case
    Forward n -> (hor + n, dep, aim)
    Down n -> (hor, dep + n, aim)
    Up n -> (hor, dep - n, aim)

runCommandWithAim :: (Int, Int, Int) -> Command -> (Int, Int, Int)
runCommandWithAim (hor, dep, aim) = \case
    Forward n -> (hor + n, dep + aim * n, aim)
    Down n -> (hor, dep, aim + n)
    Up n -> (hor, dep, aim - n)
