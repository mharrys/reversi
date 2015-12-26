module SquareSpec where

import Test.Hspec

import Piece
import Square

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should be represented as a string" $ do
        show (Square Nothing) `shouldBe` "."
        show (Square (Just Black)) `shouldBe` show Black
        show (Square (Just White)) `shouldBe` show White
