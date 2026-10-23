module Exec.EnvSpec (spec) where

import Data.Either (isLeft)
import Exec.Env
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

  describe "populateLanguage" $ do
    it "defaults to the language from CLI args, if available" $ do
      populateLanguage (Just "de-DE") fullyPopulatedEnv
        `shouldBe` Right (Just "de-DE")

    it "defaults to language from env var, if missing from CLI args" $ do
      populateLanguage Nothing fullyPopulatedEnv
        `shouldBe` Right (Just "it-IT")

    it "defaults to language from file contents, if missing from both CLI args and env var" $ do
      populateLanguage Nothing languageOnlyInFile
        `shouldBe` Right (Just "fr")

    it "returns Nothing (the API's default language) if missing everywhere" $ do
      populateLanguage Nothing emptyEnv
        `shouldBe` Right Nothing

    it "accepts a bare two-letter language code" $ do
      populateLanguage (Just "it") emptyEnv
        `shouldBe` Right (Just "it")

    it "accepts a language code with a country code" $ do
      populateLanguage (Just "pt-BR") emptyEnv
        `shouldBe` Right (Just "pt-BR")

    it "returns an Error for a language name rather than a code" $ do
      populateLanguage (Just "italian") emptyEnv
        `shouldSatisfy` isLeft

    it "returns an Error for an underscore separator" $ do
      populateLanguage (Just "it_IT") emptyEnv
        `shouldSatisfy` isLeft

    it "returns an Error for a lowercase country code" $ do
      populateLanguage (Just "it-it") emptyEnv
        `shouldSatisfy` isLeft

    it "returns an Error for an unknown language code" $ do
      populateLanguage (Just "zz") emptyEnv
        `shouldSatisfy` isLeft

    it "validates a language from the env var, too" $ do
      populateLanguage Nothing badLanguageInEnvVar
        `shouldSatisfy` isLeft

fullyPopulatedEnv :: Env
fullyPopulatedEnv =
  Env
    { apiKeyEnvVar = Just "envVarKey",
      apiKeyFile = Just "fileKey",
      languageEnvVar = Just "it-IT",
      languageFile = Just "fr"
    }

keyOnlyInFile :: Env
keyOnlyInFile = emptyEnv {apiKeyFile = Just "fileKey"}

languageOnlyInFile :: Env
languageOnlyInFile = emptyEnv {languageFile = Just "fr"}

badLanguageInEnvVar :: Env
badLanguageInEnvVar = emptyEnv {languageEnvVar = Just "italiano"}

emptyEnv :: Env
emptyEnv = Env {apiKeyEnvVar = Nothing, apiKeyFile = Nothing, languageEnvVar = Nothing, languageFile = Nothing}
