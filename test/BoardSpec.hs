module BoardSpec where

import Test.Hspec

import Coord
import Board
import Piece

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    context "node" $ do
        let empty   = node Nothing
            piece p = node $ Just p
            node  x = (Coord 0 0, x)
        it "should determine if occupied by piece" $ do
            isOccupied empty `shouldBe` False
            isOccupied (piece Black) `shouldBe` True
            isOccupiedBy empty Black `shouldBe` False
            isOccupiedBy (piece Black) Black `shouldBe` True
            isOccupiedBy (piece White) Black `shouldBe` False
        it "should swap occupied piece" $ do
            swapNode empty `shouldBe` empty
            swapNode (piece Black) `shouldBe` piece White
            swapNode (piece White) `shouldBe` piece Black
    context "standard board" $ do
        let b = standardBoard
        it "should have 64 nodes" $
            length (getNodes b) `shouldBe` 64
        it "should have 4 occupied nodes" $
            length (getOccupiedNodes b) `shouldBe` 4
        it "should have 60 unoccupied nodes" $
            length (getUnoccupiedNodes b) `shouldBe` 60
        it "should have correct piece placement" $ do
            hasPiece b 3 3 White
            hasPiece b 4 4 White
            hasPiece b 3 4 Black
            hasPiece b 4 3 Black

hasPiece :: Board -> Int -> Int -> Piece -> Expectation
hasPiece b x y p = snd (getNode b (Coord x y)) `shouldBe` Just p
