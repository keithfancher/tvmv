module Tvmv
  ( Tvmv,
    Logger,
    mkTvmv,
    runTvmv,
    liftEither,
  )
where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Writer (WriterT (..), runWriterT)
import Error (Error)
import Rename (RenameOp)

-- Wrap the transformer stack!
--
-- Note that order of these matters. Specifically, we want access to the
-- accumulated Writer values even in the case of failure/Left, so we can log
-- what *did* succeed in the case of a partial failure.
type Tvmv a = ExceptT Error (WriterT [RenameOp] IO) a

type Logger = [RenameOp] -> IO ()

-- Init a Tvmv value from an IO of an Either, with empty Writer values.
mkTvmv :: IO (Either Error a) -> Tvmv a
mkTvmv m = ExceptT $ WriterT $ do
  eitherVal <- m
  return (eitherVal, []) -- wrap the value and empty writer/accumulator list

-- Pull out the IO of Either from a Tvmv. Log the resulting Writer values using
-- the given Logger function.
runTvmv :: Tvmv a -> Logger -> IO (Either Error a)
runTvmv m logResults = do
  let writerT = runExceptT m
  (retVal, writerValues) <- runWriterT writerT
  logResults writerValues
  return retVal

-- Admittedly, I only vaguely understand why this works :')
--
-- There's a `liftEither` function in the `mtl` library as well, but I ALSO
-- don't fully get what the difference is between that and `transformers`
-- (which is what I'm using here).
liftEither :: Either Error a -> Tvmv a
liftEither = ExceptT . return
