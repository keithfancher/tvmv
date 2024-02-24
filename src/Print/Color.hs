module Print.Color
  ( Colorized (..),
    ColorText,
    red,
    yellow,
    green,
    mono,
    asError,
    asSuccess,
    asWarning,
    printError,
    printSuccess,
    printWarning,
    printColor,
    printColorLn,
    uncolor,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import System.Console.ANSI qualified as ANSI

-- Red, green, yellow, "no color"/"neutral"
data Color = R | G | Y | N

data ColorString = ColorString Color Text

newtype ColorText = ColorText [ColorString]

instance Semigroup ColorText where
  (<>) (ColorText a) (ColorText b) = ColorText $ a <> b

instance Monoid ColorText where
  mempty = ColorText []

-- Makes `OverloadedStrings` work, magically converts string literals.
-- Allows stuff like:
--     red "red text " <> "regular non-colored text"
instance IsString ColorText where
  fromString s = mono $ pack s

red :: Text -> ColorText
red = colorText R

yellow :: Text -> ColorText
yellow = colorText Y

green :: Text -> ColorText
green = colorText G

-- Monochrome, aka no color added.
mono :: Text -> ColorText
mono = colorText N

colorText :: Color -> Text -> ColorText
colorText c t = ColorText [ColorString c t]

uncolor :: ColorText -> Text
uncolor (ColorText colorStrings) = mconcat $ map uncolorString colorStrings
  where
    uncolorString (ColorString _ t) = t

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

printColorString :: (MonadIO m) => ColorString -> m ()
printColorString (ColorString c t) = liftIO $ setColor c $ TIO.putStr t
  where
    setColor R = withColor ANSI.Red
    setColor Y = withColor ANSI.Yellow
    setColor G = withColor ANSI.Green
    setColor N = id

printColor :: (MonadIO m) => ColorText -> m ()
printColor (ColorText []) = liftIO $ return ()
printColor (ColorText [cs]) = printColorString cs
printColor (ColorText multiString) = mapM_ printColorString multiString

printColorLn :: (MonadIO m) => ColorText -> m ()
printColorLn = withNewline . printColor

printError :: (MonadIO m) => Text -> m ()
printError msg = asError $ TIO.putStrLn msg

printSuccess :: (MonadIO m) => Text -> m ()
printSuccess msg = asSuccess $ TIO.putStrLn msg

printWarning :: (MonadIO m) => Text -> m ()
printWarning msg = asWarning $ TIO.putStrLn msg

asError :: (MonadIO m) => IO () -> m ()
asError = withColor ANSI.Red

asSuccess :: (MonadIO m) => IO () -> m ()
asSuccess = withColor ANSI.Green

asWarning :: (MonadIO m) => IO () -> m ()
asWarning = withColor ANSI.Yellow

-- There are two brightness options: `Vivid` and `Dull`. Both are fine in Linux
-- and Mac, but sometimes in Windows (Win10 in particular?), the dull variants
-- are SO dull as to be borderline-unreadable. So vivid it is :')
fgColor :: ANSI.Color -> [ANSI.SGR]
fgColor c = [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]

withColor :: (MonadIO m) => ANSI.Color -> IO () -> m ()
withColor c = withSGR (fgColor c)

-- Do an IO action with the given SGR, then reset.
withSGR :: (MonadIO m) => [ANSI.SGR] -> IO () -> m ()
withSGR sgr act = liftIO $ do
  ANSI.setSGR sgr
  act
  ANSI.setSGR [ANSI.Reset]

-- Do an IO action, then print a "\n" afterward.
withNewline :: (MonadIO m) => m () -> m ()
withNewline act = act >> liftIO (putStrLn "")
