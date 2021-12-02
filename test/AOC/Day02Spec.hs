module AOC.Day02Spec (spec) where

import AOC (day02, parse, part1, part2)
import Control.Monad ((<=<))
import Test.Hspec

spec :: Spec
spec = do
    describe "day02 part1" $ do
        it "location product without aim" $ do
            go1 input `shouldBe` Just 150
    describe "day02 part2" $ do
        it "location product with aim" $ do
            go2 input `shouldBe` Just 900

go1 :: String -> Maybe Int
go1 = part1 day02 <=< parse day02

go2 :: String -> Maybe Int
go2 = part2 day02 <=< parse day02

input :: String
input =
    unlines
        [ "forward 5"
        , "down 5"
        , "forward 8"
        , "up 3"
        , "down 8"
        , "forward 2"
        ]
