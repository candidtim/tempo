module Main where

import System.Environment
import Control.Monad

import Config
import Jira
import Git


main :: IO ()
main = do
  gitArgs <- getArgs
  config <- readConfig
  user <- getGitUser
  gitLog <- getGitLog config user gitArgs
  let proposedWorkLog = calculateWorkLog gitLog
  logWork config proposedWorkLog
