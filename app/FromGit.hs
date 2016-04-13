module Main where

import System.Environment
import Data.Either.Utils

import Config
import Git
import Worklog
import CLI

main :: IO ()
main = do
  args <- getArgs
  user <- getGitUser
  config <- forceEither <$> readConfig
  -- FIXME: user reader monad to read from config?
  gitLog <- getGitLog config user args
  workLog <- calculateWorkLog config gitLog
  case workLog of
    [] -> putStrLn "Nothing to log! Go get to work!"
    _  -> submitLogInteractive config workLog
