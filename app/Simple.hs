module Simple where

import System.Environment
import Control.Monad

import Data.Time

import Config
import Jira
import Git


main :: IO ()
main = do
  gitArgs <- getArgs
  config <- readConfig
  let proposedWorkLog = [WorkLog (fromGregorian 2016 1 1) "HB-123" 3.0]
  confirmed <- askToConfirm proposedWorkLog
  when confirmed $ do
    logWork config proposedWorkLog

askToConfirm :: [WorkLog] -> IO Bool
askToConfirm workLog = do
  putStrLn "Will log following items:"
  mapM_ (putStrLn . show) workLog
  putStrLn "OK? (y/n)"
  r <- getLine
  return $ r == "y"
