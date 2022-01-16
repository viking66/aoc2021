module AOC.Day06Spec (spec) where

import AOC (day06, parse, part1, part2)
import Control.Monad ((<=<))
import Test.Hspec

spec :: Spec
spec = do
    describe "day06 part1" $ do
        it "fish after 80 days" $ do
            go1 input `shouldBe` Just 5934
    describe "day06 part2" $ do
        it "fish after 256 days" $ do
            go2 input `shouldBe` Just 26984457539

go1 :: String -> Maybe Int
go1 = part1 day06 <=< parse day06

go2 :: String -> Maybe Int
go2 = part2 day06 <=< parse day06

input :: String
input = "3,4,3,1,2"
