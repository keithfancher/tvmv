module Main (main) where

import Args (cliOptParser)
import Env (populateEnv)
import Execute (execCommand, run)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser cliOptParser
  env <- populateEnv
  result <- run $ execCommand env command
  print result
