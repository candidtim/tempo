module CLI
( submitLogInteractive
) where

import Control.Monad
import Text.Printf

import Config
import Jira


askToConfirm :: [WorkLog] -> IO Bool
askToConfirm workLog = do
  putStrLn "Will log following items:"
  mapM_ (putStrLn . sitem) workLog
  putStrLn "OK? (y/n)"
  r <- getLine
  return $ r == "y"

sitem :: WorkLog -> String
sitem workLog = " * " ++ show workLog

submitLogInteractive :: Config -> [WorkLog] -> IO ()
submitLogInteractive config workLog = do
  confirmed <- askToConfirm workLog
  when confirmed $ do
    logWork config workLog
    printf "Work logged. Find your timesheet at https://%s/plugins/servlet/ac/is.origo.jira.tempo-plugin/tempo-my-work\n" (getJiraHost config)
  unless confirmed $
    putStrLn "Abort. Nothing logged."
