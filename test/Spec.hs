import Test.Hspec

import Game

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "should kill cell with one live neighbor" $ do
      let world = World [[Dead, Dead, Dead],
                         [Dead, Live, Dead],
                         [Live, Dead, Dead]]
      (updateCell world 1 1) `shouldBe` Dead
    it "should survive cell with two live neighbors" $ do
      let world = World [[Live, Dead, Live],
                         [Dead, Live, Dead],
                         [Dead, Dead, Dead]]
      (updateCell world 1 1) `shouldBe` Live
    it "should survive cell with three live neighbors" $ do
      let world = World [[Live, Dead, Live],
                         [Dead, Live, Dead],
                         [Dead, Live, Dead]]
      (updateCell world 1 1) `shouldBe` Live
    it "should kill cell with four live neighbors" $ do
      let world = World [[Live, Dead, Live],
                         [Dead, Live, Dead],
                         [Live, Live, Dead]]
      (updateCell world 1 1) `shouldBe` Dead
    it "should not resurrect cell with two live neighbors" $ do
      let world = World [[Dead, Dead, Dead],
                         [Dead, Dead, Live],
                         [Dead, Dead, Live]]
      (updateCell world 1 1) `shouldBe` Dead
    it "should resurrect cell with three live neighbors" $ do
      let world = World [[Live, Dead, Dead],
                         [Dead, Dead, Live],
                         [Live, Dead, Dead]]
      (updateCell world 1 1) `shouldBe` Live
    it "should not resurrect cell with four live neighbors" $ do
      let world = World [[Dead, Live, Live],
                         [Dead, Dead, Live],
                         [Dead, Live, Dead]]
      (updateCell world 1 1) `shouldBe` Dead
