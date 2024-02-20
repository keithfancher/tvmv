module Print.Color
  ( printError,
    printSuccess,
    printWarning,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)

printError :: (MonadIO m) => String -> m ()
printError msg = withSGR (fgColor Red) $ putStrLn msg

printSuccess :: (MonadIO m) => String -> m ()
printSuccess msg = withSGR (fgColor Green) $ putStrLn msg

printWarning :: (MonadIO m) => String -> m ()
printWarning msg = withSGR (fgColor Yellow) $ putStrLn msg

fgColor :: Color -> [SGR]
fgColor c = [SetColor Foreground Dull c]

-- Do an IO action with the given SGR, then reset.
withSGR :: (MonadIO m) => [SGR] -> IO () -> m ()
withSGR sgr act = liftIO $ do
  setSGR sgr
  act
  setSGR [Reset]
