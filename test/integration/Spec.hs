import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mv command" $ do
    it "TODOs a TODO" $ do
      True `shouldBe` True
