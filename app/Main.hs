module Main (main) where

import Args (cliOptParser)
import Execute (Env (..), execCommand, run)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser cliOptParser
  result <- run $ execCommand defaultEnv command
  print result
  where
    defaultEnv = Env Nothing Nothing -- TODO: populate Env!
