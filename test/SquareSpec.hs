module SquareSpec where

import Test.Hspec

import Piece
import Square

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should be represented as a string" $ do
        show Empty `shouldBe` "."
        show (Occupied Black) `shouldBe` show Black
        show (Occupied White) `shouldBe` show White
    it "should determine if occupied by piece" $ do
        occupied Empty `shouldBe` False
        occupied (Occupied Black) `shouldBe` True
        occupiedBy Empty Black `shouldBe` False
        occupiedBy (Occupied Black) Black `shouldBe` True
        occupiedBy (Occupied White) Black `shouldBe` False
    it "should flip occupied piece" $ do
        flipPiece Empty `shouldBe` Empty
        flipPiece (Occupied Black) `shouldBe` Occupied White
        flipPiece (Occupied White) `shouldBe` Occupied Black
