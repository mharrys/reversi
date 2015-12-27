module RulesSpec where

import Test.Hspec

import Board
import Coord
import Move
import Piece
import Square
import Rules

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "standard board" $
        context "initial state" $ do
            it "should return correct captures on initial state" $ do
                nodesToFlip (Move Black (Coord 3 2)) standardBoard `shouldBe` [(Coord 3 3, Occupied White)]
                nodesToFlip (Move Black (Coord 2 3)) standardBoard `shouldBe` [(Coord 3 3, Occupied White)]
                nodesToFlip (Move Black (Coord 5 4)) standardBoard `shouldBe` [(Coord 4 4, Occupied White)]
                nodesToFlip (Move Black (Coord 4 5)) standardBoard `shouldBe` [(Coord 4 4, Occupied White)]
                nodesToFlip (Move White (Coord 2 4)) standardBoard `shouldBe` [(Coord 3 4, Occupied Black)]
                nodesToFlip (Move White (Coord 3 5)) standardBoard `shouldBe` [(Coord 3 4, Occupied Black)]
                nodesToFlip (Move White (Coord 4 2)) standardBoard `shouldBe` [(Coord 4 3, Occupied Black)]
                nodesToFlip (Move White (Coord 5 3)) standardBoard `shouldBe` [(Coord 4 3, Occupied Black)]
                nodesToFlip (Move White (Coord 0 0)) standardBoard `shouldBe` []
                nodesToFlip (Move White (Coord 3 2)) standardBoard `shouldBe` []
                nodesToFlip (Move Black (Coord 2 4)) standardBoard `shouldBe` []
            it "should have valid moves for both players" $ do
                hasValidMove White standardBoard `shouldBe` True
                hasValidMove Black standardBoard `shouldBe` True
