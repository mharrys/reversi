module CoordSpec where

import Control.Exception (evaluate)

import Test.Hspec
import Test.QuickCheck

import Coord

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should be represented as a string" $ do
        string 0 0 "a1"
        string 3 4 "e4"
        string 7 7 "h8"
        string 13 20 "u14"
    it "should accept valid input" $ do
        coord "a1" 0 0
        coord "z1" 0 25
        coord "a26" 25 0
        coord "e4" 3 4
    it "should not accept invalid input" $ do
        nothing "a0"
        nothing "aa1"
        nothing "1a"
        nothing "g1."
        nothing "c1.0"
        nothing "11"
        nothing "gl"
        nothing ""

string :: Int -> Int -> String -> Expectation
string x y s = show (Coord x y) `shouldBe` s

coord :: String -> Int -> Int -> Expectation
coord s x y = readMaybeCoord s `shouldBe` Just (Coord x y)

nothing :: String -> Expectation
nothing s = readMaybeCoord s `shouldBe` Nothing
