module Main (main) where

import Args (cliOptParser)
import Domain.Error (printErrorMessage)
import Env (populateEnv)
import Execute (execCommand)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser cliOptParser
  env <- populateEnv
  result <- execCommand env command
  case result of
    Left e -> printErrorMessage e
    Right _ -> return ()
