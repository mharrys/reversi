module BoardSpec where

import Test.Hspec

import Coord
import Board
import Square
import Piece

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "standard board" $ do
        let b = standardBoard
        it "should have 64 cells" $
            length (cells b) `shouldBe` 64
        it "should have 4 occupied cells" $
            length (occupiedCells b) `shouldBe` 4
        it "should have 60 unoccupied cells" $
            length (unoccupiedCells b) `shouldBe` 60
        it "should have correct piece placement" $ do
            hasPiece b 3 3 White
            hasPiece b 4 4 White
            hasPiece b 3 4 Black
            hasPiece b 4 3 Black

hasPiece :: Board -> Int -> Int -> Piece -> Expectation
hasPiece b x y p = snd (cell b (Coord x y)) `shouldBe` Occupied p
