module Tvmv
  ( Tvmv,
    Logger,
    mkTvmv,
    runTvmv,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Writer (WriterT (..), runWriterT)
import Control.Monad.Writer (MonadWriter)
import Error (Error)
import Rename (RenameResult)

-- Wrap the transformer stack!
--
-- Note that order of these matters. Specifically, we want access to the
-- accumulated Writer values even in the case of failure/Left, so we can log
-- what *did* succeed in the case of a partial failure.
-- type Tvmv a = ExceptT Error (WriterT [RenameResult] IO) a
newtype Tvmv a = Tvmv (ExceptT Error (WriterT [RenameResult] IO) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO, -- TODO: do I actually need all these?
      MonadError Error,
      MonadWriter [RenameResult]
    )

-- Given a list of results, "log" them, whatever that might mean.
type Logger = [RenameResult] -> IO ()

-- Init a Tvmv value from an IO of an Either, with empty Writer values.
mkTvmv :: IO (Either Error a) -> Tvmv a
mkTvmv m = Tvmv $ ExceptT $ WriterT $ do
  eitherVal <- m
  return (eitherVal, []) -- wrap the value and empty writer/accumulator list

-- Pull out the IO of Either from a Tvmv. Log the resulting Writer values using
-- the given Logger function.
runTvmv :: Logger -> Tvmv a -> IO (Either Error a)
runTvmv logResults (Tvmv exceptT) = do
  let writerT = runExceptT exceptT
  (retVal, writerValues) <- runWriterT writerT
  logResults writerValues
  return retVal
