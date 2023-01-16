import API.Stub (testAPI)
import Command (Command (..), MvOptions (..), SearchKey (..), UndoOptions (..))
import Data.Text.IO qualified as TIO
import Domain.Error (Error)
import Execute (Env (..), execCommandWithAPI, selectRunner)
import File (InFiles (..), sortCaseInsensitive)
import Log (getLatestLog)
import System.Directory (createDirectoryIfMissing, listDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

main :: IO ()
main = hspec spec

-- Two simple integration tests. Note: they need to be run in order. (The
-- `undo` test undoes the work done by the `mv` test.)
--
-- We first create some test files in a temp directory, then call `mv` on them,
-- asserting on the resulting renamed files. Then we call `undo` to reset the
-- operation and assert again. Final cleanup is to simply remove the test
-- directory and resulting log file.
--
-- Note that these are "integration" tests in the sense that they're not pure,
-- and they depend on an environment in which to operate (the filesystem).
-- However, these tests do NOT connect to an external API -- that part's
-- stubbed out.
--
-- These tests should be safe/deterministic enough to run automatically, but
-- still will exercise most of our IO code, which is the idea.
spec :: Spec
spec = do
  describe "mv" mvSpec
  describe "undo" undoSpec

mvSpec :: Spec
mvSpec = before_ testSetup $ do
  -- note running `mvSetup` BEFORE this test
  describe "mv command" $ do
    it "renames a directory of files according to (mock) API data" $ do
      runTestCommand mvCommand `shouldReturn` Right ()
      dirContentsShouldBe expectedResults

undoSpec :: Spec
undoSpec = after_ cleanup $ do
  -- ...and cleaning up AFTER this one
  describe "undo command" $ do
    it "reverts the most recently-run mv operation" $ do
      runTestCommand undoCommand `shouldReturn` Right ()
      dirContentsShouldBe initialFiles -- should be back where we started

-- Helper to run a command with our fake API.
runTestCommand :: Command -> IO (Either Error ())
runTestCommand command = run $ execCommandWithAPI emptyEnv testAPI command
  where
    run = selectRunner command

-- Helper for asserting against the contents of the integration test temporary
-- directory.
dirContentsShouldBe :: [FilePath] -> Expectation
dirContentsShouldBe expected = do
  let newContents = sortCaseInsensitive <$> listDirectory testTempDir
  newContents `shouldReturn` expected

mvCommand :: Command
mvCommand = Mv mvOptions

emptyEnv :: Env
emptyEnv = Env {apiKeyEnvVar = Nothing, apiKeyFile = Nothing}

mvOptions :: MvOptions
mvOptions =
  MvOptions
    { apiKey = Just "ExistsButDoesntMatter",
      force = True, -- do not pause to confirm
      noLog = False, -- we want a log!
      allowPartial = False, -- don't allow partial matches
      searchKey = Id 790, -- Poirot! by ID
      seasonNum = 12,
      seasonFiles = Dir testTempDir
    }

undoCommand :: Command
undoCommand = Undo undoOptions

undoOptions :: UndoOptions
undoOptions = UndoOptions {force = True, logFile = Nothing}

-- Run before our `mv` integration test. Creates the files we'll be working
-- with in a temporary directory.
testSetup :: IO ()
testSetup = do
  putStrLn "Removing any pre-existing test files..."
  removePathForcibly testTempDir
  putStrLn $ "Creating files for test run in directory " <> testTempDir <> "..."
  touchFiles

-- Create our initial files. The fact that they're empty doesn't matter for us.
touchFiles :: IO ()
touchFiles = do
  createDirectoryIfMissing False testTempDir
  let files = map (testTempDir </>) initialFiles
  mapM_ touch files
  where
    touch f = TIO.writeFile f ""

-- Remove the directory we created, everything inside, and also the most recent
-- log file, if one exists.
cleanup :: IO ()
cleanup = do
  putStrLn "Cleaning up test data..."
  removePathForcibly testTempDir
  putStrLn "Removing log file..."
  removeLatestLogFile
  putStrLn "Complete!"

removeLatestLogFile :: IO ()
removeLatestLogFile = do
  pwdFiles <- listDirectory "."
  mapM_ removePathForcibly (getLatestLog pwdFiles) -- if one doesn't exist, do nothing

testTempDir :: FilePath
testTempDir = "it-test-tmp"

initialFiles :: [FilePath]
initialFiles = ["ep1.mp4", "Ep2.mp4", "EP3.mp4", "ep4.mp4"]

expectedResults :: [FilePath]
expectedResults =
  [ "Agatha Christie's Poirot - 12x01 - Three Act Tragedy.mp4",
    "Agatha Christie's Poirot - 12x02 - Hallowe'en Party.mp4",
    "Agatha Christie's Poirot - 12x03 - Murder on the Orient Express.mp4",
    "Agatha Christie's Poirot - 12x04 - The Clocks.mp4"
  ]
