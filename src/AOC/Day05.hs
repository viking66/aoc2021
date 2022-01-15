module AOC.Day05 (day05) where

import AOC.Types
import Control.Applicative (liftA2)
import qualified Data.MultiSet as MultiSet
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parseMaybe)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

day05 :: Solution Coords Int Int
day05 =
    Solution
        { parse = fmap mkCoords . parseMaybe (linesP <* eof)
        , part1 = \(Coords vs hs _) -> Just $ countOverlap (vs <> hs)
        , part2 = \(Coords vs hs ds) -> Just $ countOverlap (vs <> hs <> ds)
        }

type Parser = Parsec Void String

data Line = Line Coord Coord
    deriving (Show)

data Coords = Coords [Coord] [Coord] [Coord]

data Coord = Coord Int Int
    deriving (Show, Eq, Ord)

linesP :: Parser [Line]
linesP = many lineP
  where
    lineP = liftA2 Line (coordP <* string " -> ") (coordP <* newline)
    coordP = liftA2 Coord (decimal <* char ',') decimal

mkCoords :: [Line] -> Coords
mkCoords ls = Coords (allCoords isVertical) (allCoords isHorizontal) (allCoords isDiagonal)
  where
    allCoords = concatMap allCoords' . flip filter ls
    allCoords' xs@(Line (Coord x y) _) =
        let a = scale $ getX xs
            b = scale $ getY xs
         in [Coord (x + a * n) (y + b * n) | n <- [0 .. absFn max xs]]
    scale (a, b)
        | a < b = 1
        | a > b = -1
        | otherwise = 0
    isVertical = uncurry (==) . getX
    isHorizontal = uncurry (==) . getY
    isDiagonal = absFn (==)
    getX (Line (Coord x1 _) (Coord x2 _)) = (x1, x2)
    getY (Line (Coord _ y1) (Coord _ y2)) = (y1, y2)
    absFn f ln = abs (uncurry (-) $ getX ln) `f` abs (uncurry (-) $ getY ln)

countOverlap :: [Coord] -> Int
countOverlap =
    MultiSet.foldOccur (\_ n -> if n >= 2 then succ else id) 0
        . MultiSet.fromList
