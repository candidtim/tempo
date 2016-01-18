module Main where

import System.Environment
import Control.Monad

import Lib


main :: IO ()
main = do
  gitArgs <- getArgs
  user <- getGitUser
  issuePattern <- getIssuePattern
  gitLog <- getGitLog user gitArgs issuePattern
  let proposedWorkLog = calculateWorkLog gitLog
  confirmed <- askToConfirm proposedWorkLog
  when (confirmed) $ do
    logWork proposedWorkLog
