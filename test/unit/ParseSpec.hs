module ParseSpec (spec) where

import Parse
import Test.Hspec

spec :: Spec
spec = do
  describe "parseFilename" $ do
    it "parses all well-formed filenames" $ do
      mapM_ testResultSet inputOutputWellFormed

testResultSet :: (String, SeasonEpNum) -> Expectation
testResultSet (input, expected) = parseFilename input `shouldBe` Right expected

inputOutputWellFormed :: [(String, SeasonEpNum)]
inputOutputWellFormed =
  [ ("1x21", SeasonEpNum {seasonNum = 1, episodeNum = 21}),
    ("001x021", SeasonEpNum {seasonNum = 1, episodeNum = 21})
  ]
