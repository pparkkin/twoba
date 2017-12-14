import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "twoba" $ do
    it "should have implemented test cases" $ do
      False `shouldBe` True
