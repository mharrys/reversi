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
        show (Coord 0 0) `shouldBe` "a1"
        show (Coord 3 4) `shouldBe` "e4"
        show (Coord 7 7) `shouldBe` "h8"
        show (Coord 13 20) `shouldBe` "u14"
    it "should accept valid input" $ do
        readMaybeCoord "a1" `shouldBe` Just (Coord 0 0)
        readMaybeCoord "z1" `shouldBe` Just (Coord 0 25)
        readMaybeCoord "a26" `shouldBe` Just (Coord 25 0)
        readMaybeCoord "e4" `shouldBe` Just (Coord 3 4)
    it "should not accept invalid input" $ do
        readMaybeCoord "a0" `shouldBe` Nothing
        readMaybeCoord "aa1" `shouldBe` Nothing
        readMaybeCoord "1a" `shouldBe` Nothing
        readMaybeCoord "g1." `shouldBe` Nothing
        readMaybeCoord "c1.0" `shouldBe` Nothing
        readMaybeCoord "11" `shouldBe` Nothing
        readMaybeCoord "gl" `shouldBe` Nothing
        readMaybeCoord "" `shouldBe` Nothing
