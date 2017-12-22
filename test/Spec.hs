import Test.Hspec

import Game

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "should have some test cases" $ do
      False `shouldBe` True
