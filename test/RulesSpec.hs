module RulesSpec where

import Test.Hspec

import Board
import Coord
import Move
import Piece
import Rules

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "standard board" $ do
        let b = standardBoard
        context "initial state" $ do
            it "should return correct captures on initial state" $ do
                cmpSwaps Black 3 2 b [Coord 3 3]
                cmpSwaps Black 2 3 b [Coord 3 3]
                cmpSwaps Black 5 4 b [Coord 4 4]
                cmpSwaps Black 4 5 b [Coord 4 4]
                cmpSwaps White 2 4 b [Coord 3 4]
                cmpSwaps White 3 5 b [Coord 3 4]
                cmpSwaps White 4 2 b [Coord 4 3]
                cmpSwaps White 5 3 b [Coord 4 3]
                cmpSwaps White 0 0 b []
                cmpSwaps White 3 2 b []
                cmpSwaps Black 2 4 b []
            it "should have valid moves for both players" $ do
                hasValidMove White b `shouldBe` True
                hasValidMove Black b `shouldBe` True

cmpSwaps :: Piece -> Int -> Int -> Board -> [Coord] -> Expectation
cmpSwaps p x y b cs = nodesToSwap move b `shouldBe` expected
  where
    move     = Move p (Coord x y)
    expected = zip cs $ replicate (length cs) (Just (opponent p))
