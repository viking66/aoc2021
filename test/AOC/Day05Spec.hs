module AOC.Day05Spec (spec) where

import AOC (day05, parse, part1, part2)
import Control.Monad ((<=<))
import Test.Hspec

spec :: Spec
spec = do
    describe "day05 part1" $ do
        it "vertical and horizontal lines" $ do
            go1 input `shouldBe` Just 5
    describe "day05 part2" $ do
        it "vertical, horizontal, and diagonal lines" $ do
            go2 input `shouldBe` Just 12

go1 :: String -> Maybe Int
go1 = part1 day05 <=< parse day05

go2 :: String -> Maybe Int
go2 = part2 day05 <=< parse day05

input :: String
input =
    unlines
        [ "0,9 -> 5,9"
        , "8,0 -> 0,8"
        , "9,4 -> 3,4"
        , "2,2 -> 2,1"
        , "7,0 -> 7,4"
        , "6,4 -> 2,0"
        , "0,9 -> 2,9"
        , "3,4 -> 1,4"
        , "0,0 -> 8,8"
        , "5,5 -> 8,2"
        ]
