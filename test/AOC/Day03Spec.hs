module AOC.Day03Spec (spec) where

import AOC (day03, parse, part1, part2)
import Control.Monad ((<=<))
import Test.Hspec

spec :: Spec
spec = do
    describe "day03 part1" $ do
        it "power consumption" $ do
            go1 input `shouldBe` Just 198
    describe "day03 part2" $ do
        it "life support rating" $ do
            go2 input `shouldBe` Just 230

go1 :: String -> Maybe Int
go1 = part1 day03 <=< parse day03

go2 :: String -> Maybe Int
go2 = part2 day03 <=< parse day03

input :: String
input =
    unlines
        [ "00100"
        , "11110"
        , "10110"
        , "10111"
        , "10101"
        , "01111"
        , "00111"
        , "11100"
        , "10000"
        , "11001"
        , "00010"
        , "01010"
        ]
