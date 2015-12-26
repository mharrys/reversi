module PointSpec where

import Control.Exception (evaluate)

import Test.Hspec
import Test.QuickCheck

import Point

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should be represented as a string" $ do
        property $ \x y ->
            show (Point (x :: Int) (y :: Int))
            ==
            show x ++ " " ++ show y
    it "should accept valid input" $ do
        property $ \x y ->
            (read (show x ++ " " ++ show y) :: Point)
            ==
            Point (x :: Int) (y :: Int)
    it "should not accept invalid input" $ do
        readMaybePoint "0.0 0" `shouldBe` Nothing
        readMaybePoint "1 a" `shouldBe` Nothing
        readMaybePoint "5 9 3" `shouldBe` Nothing
        readMaybePoint "8" `shouldBe` Nothing
        readMaybePoint "8 0." `shouldBe` Nothing
        readMaybePoint "x 0" `shouldBe` Nothing
        readMaybePoint "  0" `shouldBe` Nothing
