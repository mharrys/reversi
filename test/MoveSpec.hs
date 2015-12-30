module MoveSpec where

import Test.Hspec

import Reversi.Coord
import Reversi.Move

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should step west" $ do
        west (Coord 0 0) `shouldBe` Coord 0 (-1)
        west (Coord 0 (-1)) `shouldBe` Coord 0 (-2)
    it "should step east" $ do
        east (Coord 0 0) `shouldBe` Coord 0 1
        east (Coord 0 1) `shouldBe` Coord 0 2
    it "should step north" $ do
        north (Coord 0 0) `shouldBe` Coord (-1) 0
        north (Coord (-1) 0) `shouldBe` Coord (-2) 0
    it "should step south" $ do
        south (Coord 0 0) `shouldBe` Coord 1 0
        south (Coord 1 0) `shouldBe` Coord 2 0

    it "should step north west" $ do
        northWest (Coord 0 0) `shouldBe` Coord (-1) (-1)
        northWest (Coord (-1) (-1)) `shouldBe` Coord (-2) (-2)
    it "should step north east" $ do
        northEast (Coord 0 0) `shouldBe` Coord (-1) 1
        northEast (Coord (-1) 1) `shouldBe` Coord (-2) 2
    it "should step south west" $ do
        southWest (Coord 0 0) `shouldBe` Coord 1 (-1)
        southWest (Coord 1 (-1)) `shouldBe` Coord 2 (-2)
    it "should step south east" $ do
        southEast (Coord 0 0) `shouldBe` Coord 1 1
        southEast (Coord 1 1) `shouldBe` Coord 2 2

    it "should walk west" $ do
        let from = Coord 0 3
            boundary (Coord x y) = x >= 0 && y >= 0
            expected = [Coord 0 2, Coord 0 1, Coord 0 0]
        coordsInDirection from west boundary `shouldBe` expected
    it "should walk east" $ do
        let from = Coord 0 0
            boundary (Coord x y) = x >= 0 && y <= 3
            expected = [Coord 0 1, Coord 0 2, Coord 0 3]
        coordsInDirection from east boundary `shouldBe` expected
    it "should walk north" $ do
        let from = Coord 3 0
            boundary (Coord x y) = x >= 0 && y >= 0
            expected = [Coord 2 0, Coord 1 0, Coord 0 0]
        coordsInDirection from north boundary `shouldBe` expected
    it "should walk south" $ do
        let from = Coord 0 0
            boundary (Coord x y) = x <= 3 && y >= 0
            expected = [Coord 1 0, Coord 2 0, Coord 3 0]
        coordsInDirection from south boundary `shouldBe` expected

    it "should walk north west" $ do
        let from = Coord 3 3
            boundary (Coord x y) = x >= 0 && y >= 0
            expected = [Coord 2 2, Coord 1 1, Coord 0 0]
        coordsInDirection from northWest boundary `shouldBe` expected
    it "should walk north east" $ do
        let from = Coord 3 3
            boundary (Coord x y) = x >= 0 && y <= 6
            expected = [Coord 2 4, Coord 1 5, Coord 0 6]
        coordsInDirection from northEast boundary `shouldBe` expected

    it "should walk south west" $ do
        let from = Coord 3 3
            boundary (Coord x y) = x <= 6 && y >= 0
            expected = [Coord 4 2, Coord 5 1, Coord 6 0]
        coordsInDirection from southWest boundary `shouldBe` expected
    it "should walk south east" $ do
        let from = Coord 3 3
            boundary (Coord x y) = x <= 6 && y <= 6
            expected = [Coord 4 4, Coord 5 5, Coord 6 6]
        coordsInDirection from southEast boundary `shouldBe` expected
