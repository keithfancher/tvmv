module Main (main) where

import API (APIKey)
import qualified Data.Text as T
import Execute (renameSeason)
import System.Environment (getArgs)
import TVMV (runTVMV)

main :: IO ()
main = do
  args <- getArgs
  let (apiKey, query, seasonNum, dirPath) = processArgs args
  result <- runTVMV $ renameSeason apiKey query seasonNum dirPath
  print result

-- Obviously not a long-term solution...
processArgs :: [String] -> (APIKey, T.Text, Int, FilePath)
processArgs [k, q, n, path] = (T.pack k, T.pack q, read n, path)
processArgs _ = error "Usage: tvmv [APIKEY] [SHOWNAME] [SEASONNUM] [PATH]"
