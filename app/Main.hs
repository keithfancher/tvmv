module Main (main) where

import Args (cliOptParser)
import Domain.Error (Error (..))
import Env (populateEnv)
import Execute (execCommand)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser cliOptParser
  env <- populateEnv
  result <- execCommand env command
  case result of
    -- Specal case for user abort, so it's a bit "nicer":
    Left UserAbort -> putStrLn "Operation aborted! No files have been renamed."
    Left e -> putStrLn $ "Failed with: " <> show e
    Right _ -> return ()
