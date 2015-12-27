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
    context "standard board" $ do
        let b = standardBoard
        context "initial state" $ do
            it "should return correct captures on initial state" $ do
                cmpFlips Black 3 2 b [Coord 3 3]
                cmpFlips Black 2 3 b [Coord 3 3]
                cmpFlips Black 5 4 b [Coord 4 4]
                cmpFlips Black 4 5 b [Coord 4 4]
                cmpFlips White 2 4 b [Coord 3 4]
                cmpFlips White 3 5 b [Coord 3 4]
                cmpFlips White 4 2 b [Coord 4 3]
                cmpFlips White 5 3 b [Coord 4 3]
                cmpFlips White 0 0 b []
                cmpFlips White 3 2 b []
                cmpFlips Black 2 4 b []
            it "should have valid moves for both players" $ do
                hasValidMove White b `shouldBe` True
                hasValidMove Black b `shouldBe` True

cmpFlips :: Piece -> Int -> Int -> Board -> [Coord] -> Expectation
cmpFlips p x y b cs = nodesToFlip move b `shouldBe` expected
  where
    move     = Move p (Coord x y)
    expected = zip cs $ replicate (length cs) (Occupied (opponent p))
