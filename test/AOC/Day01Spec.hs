module AOC.Day01Spec (spec) where

import AOC (day01, parse, part1, part2)
import Control.Monad ((<=<))
import Test.Hspec

spec :: Spec
spec = do
    describe "day01 part1" $ do
        it "count increasing" $ do
            go1 input `shouldBe` Just 7
    describe "day01 part2" $ do
        it "triple sum count increasing" $ do
            go2 input `shouldBe` Just 5

go1 :: String -> Maybe Int
go1 = part1 day01 <=< parse day01

go2 :: String -> Maybe Int
go2 = part2 day01 <=< parse day01

input :: String
input =
    unlines
        [ "199"
        , "200"
        , "208"
        , "210"
        , "200"
        , "207"
        , "240"
        , "269"
        , "260"
        , "263"
        ]
