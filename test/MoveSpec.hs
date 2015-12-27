module MoveSpec where

import Test.Hspec

import Move

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "should step west" $ do
        west (0, 0) `shouldBe` (0, -1)
        west (0, -1) `shouldBe` (0, -2)
    it "should step east" $ do
        east (0, 0) `shouldBe` (0, 1)
        east (0, 1) `shouldBe` (0, 2)
    it "should step north" $ do
        north (0, 0) `shouldBe` (-1, 0)
        north (-1, 0) `shouldBe` (-2, 0)
    it "should step south" $ do
        south (0, 0) `shouldBe` (1, 0)
        south (1, 0) `shouldBe` (2, 0)

    it "should step north west" $ do
        northWest (0, 0) `shouldBe` (-1, -1)
        northWest (-1, -1) `shouldBe` (-2, -2)
    it "should step north east" $ do
        northEast (0, 0) `shouldBe` (-1, 1)
        northEast (-1, 1) `shouldBe` (-2, 2)
    it "should step south west" $ do
        southWest (0, 0) `shouldBe` (1, -1)
        southWest (1, -1) `shouldBe` (2, -2)
    it "should step south east" $ do
        southEast (0, 0) `shouldBe` (1, 1)
        southEast (1, 1) `shouldBe` (2, 2)

    it "should walk west" $ do
        let from = (0, 3)
            boundary (x, y) = x >= 0 && y >= 0
            expected = [(0, 2), (0, 1), (0, 0)]
        pointsInDirection from west boundary `shouldBe` expected
    it "should walk east" $ do
        let from = (0, 0)
            boundary (x, y) = x >= 0 && y <= 3
            expected = [(0, 1), (0, 2), (0, 3)]
        pointsInDirection from east boundary `shouldBe` expected
    it "should walk north" $ do
        let from = (3, 0)
            boundary (x, y) = x >= 0 && y >= 0
            expected = [(2, 0), (1, 0), (0, 0)]
        pointsInDirection from north boundary `shouldBe` expected
    it "should walk south" $ do
        let from = (0, 0)
            boundary (x, y) = x <= 3 && y >= 0
            expected = [(1, 0), (2, 0), (3, 0)]
        pointsInDirection from south boundary `shouldBe` expected

    it "should walk north west" $ do
        let from = (3, 3)
            boundary (x, y) = x >= 0 && y >= 0
            expected = [(2, 2), (1, 1), (0, 0)]
        pointsInDirection from northWest boundary `shouldBe` expected
    it "should walk north east" $ do
        let from = (3, 3)
            boundary (x, y) = x >= 0 && y <= 6
            expected = [(2, 4), (1, 5), (0, 6)]
        pointsInDirection from northEast boundary `shouldBe` expected

    it "should walk south west" $ do
        let from = (3, 3)
            boundary (x, y) = x <= 6 && y >= 0
            expected = [(4, 2), (5, 1), (6, 0)]
        pointsInDirection from southWest boundary `shouldBe` expected
    it "should walk south east" $ do
        let from = (3, 3)
            boundary (x, y) = x <= 6 && y <= 6
            expected = [(4, 4), (5, 5), (6, 6)]
        pointsInDirection from southEast boundary `shouldBe` expected
