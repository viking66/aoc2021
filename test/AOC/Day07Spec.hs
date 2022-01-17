module AOC.Day07Spec (spec) where

import AOC (day07, parse, part1, part2)
import Control.Monad ((<=<))
import Test.Hspec

spec :: Spec
spec = do
    describe "day07 part1" $ do
        it "constant rate step count" $ do
            go1 input `shouldBe` Just 37
    describe "increasing rate step count" $ do
        it "fish after 256 days" $ do
            go2 input `shouldBe` Just 168

go1 :: String -> Maybe Int
go1 = part1 day07 <=< parse day07

go2 :: String -> Maybe Int
go2 = part2 day07 <=< parse day07

input :: String
input = "16,1,2,0,4,2,7,1,2,14"
