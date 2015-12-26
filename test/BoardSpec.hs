module BoardSpec where

import Test.Hspec

import Board
import Square
import Piece

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "standard board" $ do
        it "should have 64 squares" $
            length (cells standardBoard) `shouldBe` 64
        it "should have 4 occupied squares" $
            length (occupiedSquares standardBoard) `shouldBe` 4
        it "should have 60 empty squares" $
            length (emptySquares standardBoard) `shouldBe` 60
        it "should have correct piece placement" $ do
            snd (cell standardBoard (3, 3)) `shouldBe` Occupied White
            snd (cell standardBoard (4, 4)) `shouldBe` Occupied White
            snd (cell standardBoard (3, 4)) `shouldBe` Occupied Black
            snd (cell standardBoard (4, 3)) `shouldBe` Occupied Black
