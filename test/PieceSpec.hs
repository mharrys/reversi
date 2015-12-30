module PieceSpec where

import Test.Hspec

import Reversi.Piece

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should be represented as a string" $ do
        show Black `shouldBe` "b"
        show White `shouldBe` "w"
    it "should give its opponent" $ do
        opponent Black `shouldBe` White
        opponent White `shouldBe` Black
