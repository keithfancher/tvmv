module Main (main) where

import Args (cliOptParser)
import Execute (execCommand, run)
import Options.Applicative (execParser)

main :: IO ()
main = do
  command <- execParser cliOptParser
  result <- run $ execCommand command
  print result
