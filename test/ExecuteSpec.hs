module ExecuteSpec (spec) where

import Data.Either (isLeft)
import Execute
import Test.Hspec

spec :: Spec
spec = do
  describe "populateAPIKey" $ do
    it "defaults to the key from CLI args, if available" $ do
      populateAPIKey (Just "cliArgKey") fullyPopulatedEnv
        `shouldBe` Right "cliArgKey"

    it "defaults to key from env var, if missing from CLI args" $ do
      populateAPIKey Nothing fullyPopulatedEnv
        `shouldBe` Right "envVarKey"

    it "defaults to key from file contents, if missing from both CLI args and env var" $ do
      populateAPIKey Nothing keyOnlyInFile
        `shouldBe` Right "fileKey"

    it "return an Error if key is missing everywhere" $ do
      populateAPIKey Nothing emptyEnv
        `shouldSatisfy` isLeft

fullyPopulatedEnv :: Env
fullyPopulatedEnv = Env {apiKeyEnvVar = Just "envVarKey", apiKeyFile = Just "fileKey"}

keyOnlyInFile :: Env
keyOnlyInFile = Env {apiKeyEnvVar = Nothing, apiKeyFile = Just "fileKey"}

emptyEnv :: Env
emptyEnv = Env {apiKeyEnvVar = Nothing, apiKeyFile = Nothing}
