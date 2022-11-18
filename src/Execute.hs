module Execute
  ( Env (..), -- re-export, for convenience
    execCommand,
  )
where

import Command (Command (..), noLog)
import Error (Error (..))
import Exec.Commands (renameSeason, searchByName, undoRename)
import Exec.Env (Env (..))
import Log (printAndWriteLog, printLog)
import Tvmv (Tvmv, runTvmv)

execCommand :: Env -> Command -> IO (Either Error ())
execCommand env (Mv mvOpts) = run $ renameSeason env mvOpts
  where
    run = if noLog mvOpts then runNoLog else runWithLog
execCommand env (Search searchOpts) = runNoLog $ searchByName env searchOpts
execCommand _ (Undo undoOpts) = runNoLog $ undoRename undoOpts

-- Run the Tvmv, print the results from its writer AND write a log file.
runWithLog :: Tvmv a -> IO (Either Error a)
runWithLog = runTvmv printAndWriteLog

-- Run the Tvmv, print the results ONLY. (No log file.)
runNoLog :: Tvmv a -> IO (Either Error a)
runNoLog = runTvmv printLog
