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
        property $ \x y ->
            show (Coord (x :: Int) (y :: Int))
            ==
            show x ++ " " ++ show y
    it "should accept valid input" $ do
        property $ \x y ->
            (read (show x ++ " " ++ show y) :: Coord)
            ==
            Coord (x :: Int) (y :: Int)
    it "should not accept invalid input" $ do
        readMaybeCoord "0.0 0" `shouldBe` Nothing
        readMaybeCoord "1 a" `shouldBe` Nothing
        readMaybeCoord "5 9 3" `shouldBe` Nothing
        readMaybeCoord "8" `shouldBe` Nothing
        readMaybeCoord "8 0." `shouldBe` Nothing
        readMaybeCoord "x 0" `shouldBe` Nothing
        readMaybeCoord "  0" `shouldBe` Nothing
