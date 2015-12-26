module MoveSpec where

import Test.Hspec

import Board
import Move
import Piece
import Square

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    it "should return correct captures on initial standard board" $ do
        move (Move Black (3, 2)) standardBoard `shouldBe` [((3, 3), Occupied White)]
        move (Move Black (2, 3)) standardBoard `shouldBe` [((3, 3), Occupied White)]
        move (Move Black (5, 4)) standardBoard `shouldBe` [((4, 4), Occupied White)]
        move (Move Black (4, 5)) standardBoard `shouldBe` [((4, 4), Occupied White)]
        move (Move White (2, 4)) standardBoard `shouldBe` [((3, 4), Occupied Black)]
        move (Move White (3, 5)) standardBoard `shouldBe` [((3, 4), Occupied Black)]
        move (Move White (4, 2)) standardBoard `shouldBe` [((4, 3), Occupied Black)]
        move (Move White (5, 3)) standardBoard `shouldBe` [((4, 3), Occupied Black)]
        move (Move White (0, 0)) standardBoard `shouldBe` []
        move (Move White (3, 2)) standardBoard `shouldBe` []
        move (Move Black (2, 4)) standardBoard `shouldBe` []
