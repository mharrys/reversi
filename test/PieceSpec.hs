module PieceSpec where

import Test.Hspec

import Piece

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should be represented as a string" $ do
        show Black `shouldBe` "b"
        show White `shouldBe` "w"
