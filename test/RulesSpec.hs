module RulesSpec where

import Test.Hspec

import Board
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
                nodesToFlip (Move Black (3, 2)) standardBoard `shouldBe` [((3, 3), Occupied White)]
                nodesToFlip (Move Black (2, 3)) standardBoard `shouldBe` [((3, 3), Occupied White)]
                nodesToFlip (Move Black (5, 4)) standardBoard `shouldBe` [((4, 4), Occupied White)]
                nodesToFlip (Move Black (4, 5)) standardBoard `shouldBe` [((4, 4), Occupied White)]
                nodesToFlip (Move White (2, 4)) standardBoard `shouldBe` [((3, 4), Occupied Black)]
                nodesToFlip (Move White (3, 5)) standardBoard `shouldBe` [((3, 4), Occupied Black)]
                nodesToFlip (Move White (4, 2)) standardBoard `shouldBe` [((4, 3), Occupied Black)]
                nodesToFlip (Move White (5, 3)) standardBoard `shouldBe` [((4, 3), Occupied Black)]
                nodesToFlip (Move White (0, 0)) standardBoard `shouldBe` []
                nodesToFlip (Move White (3, 2)) standardBoard `shouldBe` []
                nodesToFlip (Move Black (2, 4)) standardBoard `shouldBe` []
            it "should have valid moves for both players" $ do
                hasValidMove White standardBoard `shouldBe` True
                hasValidMove Black standardBoard `shouldBe` True
