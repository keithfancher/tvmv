module Print.Color
  ( Colorized (..),
    ColorText (..),
    asError,
    asSuccess,
    asWarning,
    printError,
    printSuccess,
    printWarning,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.String (IsString (..))
import Data.Text (Text, pack)
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

-- Makes `OverloadedStrings` work, magically converts string literals.
-- Allows stuff like:
--     R "red text" <> "regular non-colored text"
instance IsString ColorText where
  fromString s = N $ pack s

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

asError :: (MonadIO m) => IO () -> m ()
asError = withSGR (fgColor Red)

asSuccess :: (MonadIO m) => IO () -> m ()
asSuccess = withSGR (fgColor Green)

asWarning :: (MonadIO m) => IO () -> m ()
asWarning = withSGR (fgColor Yellow)

-- There are two brightness options: `Vivid` and `Dull`. Both are fine in Linux
-- and Mac, but sometimes in Windows (Win10 in particular?), the dull variants
-- are SO dull as to be borderline-unreadable. So vivid it is :')
fgColor :: Color -> [SGR]
fgColor c = [SetColor Foreground Vivid c]

-- Do an IO action with the given SGR, then reset.
withSGR :: (MonadIO m) => [SGR] -> IO () -> m ()
withSGR sgr act = liftIO $ do
  setSGR sgr
  act
  setSGR [Reset]

-- Do an IO action, then print a "\n" afterward.
withNewline :: (MonadIO m) => m () -> m ()
withNewline act = act >> liftIO (putStrLn "")
