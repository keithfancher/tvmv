module Print.Color
  ( printError,
    printSuccess,
    printWarning,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)

data ColorText
  = R String -- red
  | G String -- green
  | Y String -- yellow
  | N String -- no color, default
  | Arr [ColorText]

instance Semigroup ColorText where
  (<>) (Arr a) (Arr b) = Arr (a ++ b)
  (<>) (Arr a) c = Arr (a ++ [c])
  (<>) c (Arr a) = Arr (c : a)
  (<>) c1 c2 = Arr (c1 : [c2])

instance Monoid ColorText where
  mempty = Arr []

class Colorized a where
  colorize :: a -> ColorText -- one required function
  printColorized :: (MonadIO m) => a -> m ()
  printColorizedLn :: (MonadIO m) => a -> m ()
  printColorizedList :: (MonadIO m) => [a] -> m ()

  -- Default implementations:
  printColorized = printColor . colorize
  printColorizedLn = printColorLn . colorize
  printColorizedList = mapM_ withseps
    where
      withseps x = do
        printColorized x
        liftIO $ putStr "\n\n"

printColor :: (MonadIO m) => ColorText -> m ()
printColor (R s) = withSGR (fgColor Red) $ putStr s
printColor (Y s) = withSGR (fgColor Yellow) $ putStr s
printColor (G s) = withSGR (fgColor Green) $ putStr s
printColor (N s) = liftIO $ putStr s
printColor (Arr arr) = mapM_ printColor arr

printColorLn :: (MonadIO m) => ColorText -> m ()
printColorLn c = liftIO $ printColor c >> putStrLn ""

printError :: (MonadIO m) => String -> m ()
printError msg = printColorLn $ R msg

printSuccess :: (MonadIO m) => String -> m ()
printSuccess msg = printColorLn $ G msg

printWarning :: (MonadIO m) => String -> m ()
printWarning msg = printColorLn $ Y msg

fgColor :: Color -> [SGR]
fgColor c = [SetColor Foreground Dull c]

-- Do an IO action with the given SGR, then reset.
withSGR :: (MonadIO m) => [SGR] -> IO () -> m ()
withSGR sgr act = liftIO $ do
  setSGR sgr
  act
  setSGR [Reset]
