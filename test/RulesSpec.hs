module RulesSpec where

import Test.Hspec

import Reversi.Board
import Reversi.Coord
import Reversi.Move
import Reversi.Piece
import Reversi.Rules

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "standard board" $ do
        let b = standardBoard
        context "initial state" $ do
            it "should not allow placement on existing nodes" $ do
                cmpValid White 3 3 b False
                cmpValid Black 3 3 b False
                cmpValid White 3 4 b False
                cmpValid Black 3 4 b False
                cmpValid White 4 3 b False
                cmpValid Black 4 3 b False
                cmpValid White 4 4 b False
                cmpValid Black 4 4 b False
                cmpValid Black 3 2 b True
                cmpValid Black 2 3 b True
                cmpValid Black 5 4 b True
                cmpValid Black 4 5 b True
                cmpValid White 2 4 b True
                cmpValid White 3 5 b True
                cmpValid White 4 2 b True
                cmpValid White 5 3 b True
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
        context "game in progress" $ do
            it "should allow multiple captures in one direction" $ do
                let b = read "a1 h8 \
                    \........\
                    \...w....\
                    \...w....\
                    \...w....\
                    \...b....\
                    \........\
                    \........\
                    \........" :: Board
                cmpSwaps Black 0 3 b [ Coord 3 3
                                     , Coord 2 3
                                     , Coord 1 3
                                     ]
                hasValidMove White b `shouldBe` True
                hasValidMove Black b `shouldBe` True
            it "should allow multiple captures in multiple directions" $ do
                let b = read "a1 h8 \
                    \b.......\
                    \.w......\
                    \..w.....\
                    \...w....\
                    \.....wwb\
                    \........\
                    \........\
                    \........" :: Board
                cmpSwaps Black 4 4 b [ Coord 4 6
                                     , Coord 4 5
                                     , Coord 1 1
                                     , Coord 2 2
                                     , Coord 3 3
                                     ]
                hasValidMove White b `shouldBe` False
                hasValidMove Black b `shouldBe` True
            it "should allow captures not ending with the capturer piece" $ do
                let b = read "a1 h8 \
                    \.b......\
                    \bb..bb.w\
                    \........\
                    \b...b...\
                    \.b...b..\
                    \.bb.....\
                    \.......w\
                    \........" :: Board
                cmpValid White 0 1 b False
                cmpValid White 3 1 b False
                cmpValid White 4 0 b False
                cmpValid White 4 2 b False
                cmpValid White 5 0 b False
                cmpValid White 5 3 b False
                cmpValid White 1 6 b False
                cmpValid White 1 3 b False
                cmpValid White 2 3 b False
                hasValidMove White b `shouldBe` False
                hasValidMove Black b `shouldBe` False
        context "game over before grid is filled" $
            it "should have not more valid moves for both players" $ do
                let b = read "a1 h8 \
                    \wwwwwwww\
                    \wwwwwwww\
                    \wwwwwwww\
                    \wwwwwww.\
                    \wwwwww..\
                    \wwwwww.b\
                    \wwwwwww.\
                    \wwwwwwww" :: Board
                hasValidMove White b `shouldBe` False
                hasValidMove Black b `shouldBe` False

cmpValid :: Piece -> Int -> Int -> Board -> Bool -> Expectation
cmpValid p x y b expected = isMoveValid move b `shouldBe` expected
  where
    move = Move p (Coord x y)

cmpSwaps :: Piece -> Int -> Int -> Board -> [Coord] -> Expectation
cmpSwaps p x y b cs = nodesToSwap move b `shouldBe` expected
  where
    move     = Move p (Coord x y)
    expected = zip cs $ replicate (length cs) (Just (opponent p))
