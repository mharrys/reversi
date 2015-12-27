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
        it "should have 64 cells" $
            length (cells standardBoard) `shouldBe` 64
        it "should have 4 occupied cells" $
            length (occupiedCells standardBoard) `shouldBe` 4
        it "should have 60 unoccupied cells" $
            length (unoccupiedCells standardBoard) `shouldBe` 60
        it "should have correct piece placement" $ do
            snd (cell standardBoard (Coord 3 3)) `shouldBe` Occupied White
            snd (cell standardBoard (Coord 4 4)) `shouldBe` Occupied White
            snd (cell standardBoard (Coord 3 4)) `shouldBe` Occupied Black
            snd (cell standardBoard (Coord 4 3)) `shouldBe` Occupied Black
