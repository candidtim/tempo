module Main where

import System.Environment
import Control.Monad
import Control.Monad.Except
import Text.Printf

import Data.Either.Utils

import Config
import Jira
import Git


main :: IO ()
main = do
  gitArgs <- getArgs
  errorOrConfig <- readConfig
  let config = forceEither errorOrConfig
  user <- getGitUser
  gitLog <- getGitLog config user gitArgs
  let proposedWorkLog = calculateWorkLog gitLog
  confirmed <- askToConfirm proposedWorkLog
  when confirmed $ do
    logWork config proposedWorkLog
    printf "Work logged. Find your timesheet at https://%s/secure/TempoUserBoard!timesheet.jspa\n" (getJiraHost config)
  unless confirmed $
    putStrLn "Abort. Nothing logged."


askToConfirm :: [WorkLog] -> IO Bool
askToConfirm workLog = do
  putStrLn "Will log following items:"
  mapM_ (putStrLn . sitem) workLog
  putStrLn "OK? (y/n)"
  r <- getLine
  return $ r == "y"

sitem :: WorkLog -> String
sitem workLog = " * " ++ show workLog
