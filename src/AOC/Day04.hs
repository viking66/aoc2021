{-# LANGUAGE LambdaCase #-}

module AOC.Day04 (day04) where

import AOC.Types
import Control.Monad ((<=<))
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Safe (maximumByMay, minimumByMay)
import Text.Megaparsec (Parsec, eof, many, parseMaybe, sepBy1)
import Text.Megaparsec.Char (char, hspace, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

day04 :: Solution (Int, Int) Int Int
day04 =
    Solution
        { parse = findWins <=< parseMaybe parseBingo
        , part1 = Just . fst
        , part2 = Just . snd
        }

data Bingo = Bingo [Int] [Board Square]
    deriving (Show)

newtype Board a = Board {unBoard :: [[a]]}
    deriving (Show)

instance Functor Board where
    fmap f = Board . map (map f) . unBoard

data Square = Unmarked Int | Marked Int
    deriving (Show)

type Parser = Parsec Void String

parseBingo :: Parser Bingo
parseBingo = Bingo <$> parseNums <* space <*> parseBoards <* eof
  where
    parseNums = decimal `sepBy1` char ','
    parseBoards = parseBoard `sepBy1` newline
    parseBoard = Board <$> many parseBoardRow
    parseBoardRow = map Unmarked <$> (hspace *> (decimal `sepBy1` hspace) <* newline)

findWins :: Bingo -> Maybe (Int, Int)
findWins (Bingo ns bs) = do
    firstWin <- findWinBy minimumByMay
    lastWin <- findWinBy maximumByMay
    Just (score firstWin, score lastWin)
  where
    findWinBy f =
        fmap dropFst3
            . f (compare `on` fst3)
            $ mapMaybe (findWin 0 ns) bs
    fst3 (a, _, _) = a
    dropFst3 (_, b, c) = (b, c)
    score (n, b) = (n *) . sum . concat . unBoard $ (square id (const 0) <$> b)
    findWin _ [] _ = Nothing
    findWin c (n : ns) b =
        let b' = markSquare n b
            c' = succ c
         in bool (findWin c' ns b') (Just (c', n, b')) (isWin b')
    isWin x = let x' = unBoard x in horizontalWin x' || verticalWin x'
    horizontalWin = any (all isMarked)
    verticalWin = any (all isMarked) . transpose
    isMarked = square (const False) (const True)
    markSquare n = fmap (\s -> if n == square id id s then Marked n else s)
    square f g = \case
        Unmarked n -> f n
        Marked n -> g n
