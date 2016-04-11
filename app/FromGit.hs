module FromGit where

import System.Environment
import Data.Either.Utils

import Config
import Git
import CLI

main :: IO ()
main = do
  args <- getArgs
  user <- getGitUser
  config <- forceEither <$> readConfig
  gitLog <- getGitLog config user args
  let workLog = calculateWorkLog gitLog
  submitLogInteractive config workLog
