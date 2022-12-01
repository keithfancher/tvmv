module Main (main) where

import Args (cliOptParser)
import Domain.Error (errorMessage)
import Env (populateEnv)
import Execute (execCommand)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser cliOptParser
  env <- populateEnv
  result <- execCommand env command
  case result of
    Left e -> putStrLn $ errorMessage e
    Right _ -> return ()
