module Print.Color
  ( Colorized (..),
    ColorText (..),
    printError,
    printSuccess,
    printWarning,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)

data ColorText
  = R Text -- red
  | G Text -- green
  | Y Text -- yellow
  | N Text -- no color, default
  | Arr [ColorText]

instance Semigroup ColorText where
  (<>) (Arr a) (Arr b) = Arr (a ++ b)
  (<>) (Arr a) c = Arr (a ++ [c])
  (<>) c (Arr a) = Arr (c : a)
  (<>) c1 c2 = Arr (c1 : [c2])

class Colorized a where
  colorize :: a -> ColorText -- one required function
  printColorized :: (MonadIO m) => a -> m ()
  printColorizedLn :: (MonadIO m) => a -> m ()
  printColorizedList :: (MonadIO m) => [a] -> m ()
  printColorizedListLn :: (MonadIO m) => [a] -> m ()

  -- Default implementations:
  printColorized = printColor . colorize
  printColorizedLn = withNewline . printColorized

  printColorizedList l = liftIO iosWithSep
    where
      colorizedIOs = map printColorized l
      printSep = putStr "\n\n"
      -- Blows my mind that you can alter a list of IO actions like this!
      iosWithSep = sequence_ $ intersperse printSep colorizedIOs

  printColorizedListLn = withNewline . printColorizedList

printColor :: (MonadIO m) => ColorText -> m ()
printColor (R t) = withSGR (fgColor Red) $ TIO.putStr t
printColor (Y t) = withSGR (fgColor Yellow) $ TIO.putStr t
printColor (G t) = withSGR (fgColor Green) $ TIO.putStr t
printColor (N t) = liftIO $ TIO.putStr t
printColor (Arr arr) = mapM_ printColor arr

printColorLn :: (MonadIO m) => ColorText -> m ()
printColorLn = withNewline . printColor

printError :: (MonadIO m) => Text -> m ()
printError msg = printColorLn $ R msg

printSuccess :: (MonadIO m) => Text -> m ()
printSuccess msg = printColorLn $ G msg

printWarning :: (MonadIO m) => Text -> m ()
printWarning msg = printColorLn $ Y msg

fgColor :: Color -> [SGR]
fgColor c = [SetColor Foreground Dull c]

-- Do an IO action with the given SGR, then reset.
withSGR :: (MonadIO m) => [SGR] -> IO () -> m ()
withSGR sgr act = liftIO $ do
  setSGR sgr
  act
  setSGR [Reset]

-- Do an IO action, then print a "\n" afterward.
withNewline :: (MonadIO m) => m () -> m ()
withNewline act = act >> liftIO (putStrLn "")
