module Execute
  ( Env (..), -- re-export, for convenience
    execCommand,
  )
where

import API (APIWrapper, tmdbApiWrapper)
import Command (Command (..), noLog)
import Error (Error (..))
import Exec.Commands (renameSeason, searchByName, undoRename)
import Exec.Env (Env (..))
import Log (printAndWriteLog, printLog)
import Tvmv (Tvmv, runTvmv)

execCommand :: Env -> Command -> IO (Either Error ())
execCommand env = execCommandWithAPI env defaultApi

-- TODO: make this generic, not over Tvmv
execCommandWithAPI :: Env -> APIWrapper Tvmv -> Command -> IO (Either Error ())
execCommandWithAPI env withApi (Mv mvOpts) = run $ renameSeason env withApi mvOpts
  where
    run = if noLog mvOpts then runNoLog else runWithLog
execCommandWithAPI env withApi (Search searchOpts) = runNoLog $ searchByName env withApi searchOpts
execCommandWithAPI _ _ (Undo undoOpts) = runNoLog $ undoRename undoOpts

-- Run the Tvmv, print the results from its writer AND write a log file.
runWithLog :: Tvmv a -> IO (Either Error a)
runWithLog = runTvmv printAndWriteLog

-- Run the Tvmv, print the results ONLY. (No log file.)
runNoLog :: Tvmv a -> IO (Either Error a)
runNoLog = runTvmv printLog

defaultApi :: APIWrapper Tvmv
defaultApi = tmdbApiWrapper
