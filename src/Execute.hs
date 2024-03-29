module Execute
  ( Env (..), -- re-export, for convenience
    execCommand,
    execCommandWithAPI,
    selectRunner,
  )
where

import API (defaultAPI)
import Command (Command (..), noLog)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Writer.Class (MonadWriter)
import Domain.API (APIWrapper)
import Domain.Error (Error (..))
import Exec.Commands qualified as Commands
import Exec.Env (Env (..))
import Exec.Rename (RenameResult)
import Log (printAndWriteLog, printLog)
import Monad.Tvmv (Tvmv, runTvmv)

-- Execute a command with the default API
execCommand :: Env -> Command -> IO (Either Error ())
execCommand env command = run $ execCommandWithAPI env defaultAPI command
  where
    run = selectRunner command

-- Execute a command with any other API.
execCommandWithAPI ::
  (MonadIO m, MonadError Error m, MonadWriter [RenameResult] m) =>
  Env ->
  APIWrapper m ->
  Command ->
  m ()
execCommandWithAPI env withApi (Mv mvOpts) = Commands.mv env withApi mvOpts
execCommandWithAPI env withApi (Search searchOpts) = Commands.search env withApi searchOpts
execCommandWithAPI _ _ (Undo undoOpts) = Commands.undo undoOpts

-- Different commands have different logging behavior, and therefore different
-- runners.
selectRunner :: Command -> (Tvmv a -> IO (Either Error a))
selectRunner (Mv mvOpts) = run
  where
    run = if noLog mvOpts then runNoLog else runWithLog
selectRunner (Search _) = runNoLog
selectRunner (Undo _) = runNoLog

-- Run the Tvmv, print the results from its writer AND write a log file.
runWithLog :: Tvmv a -> IO (Either Error a)
runWithLog = runTvmv printAndWriteLog

-- Run the Tvmv, print the results ONLY. (No log file.)
runNoLog :: Tvmv a -> IO (Either Error a)
runNoLog = runTvmv printLog
