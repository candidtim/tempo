module Simple where

import System.Environment
import Text.Printf
import Control.Monad
import Control.Monad.Except

import Data.Either.Utils

import Data.Time
import Data.Time.Format

import Config
import Jira
import Git


main :: IO ()
main = do
  args <- getArgs
  errorOrConfig <- readConfig
  let config = forceEither errorOrConfig
      workLog = forceEither $ parseArgs args
  confirmed <- askToConfirm workLog
  when confirmed $ do
    logWork config [workLog]
    printf "Work logged. Find your timesheet at https://%s/secure/TempoUserBoard!timesheet.jspa\n" (getJiraHost config)
  unless confirmed $
    putStrLn "Abort. Nothing logged."

askToConfirm :: WorkLog -> IO Bool
askToConfirm workLog = do
  putStrLn "Will log following items:"
  print workLog
  putStrLn "OK? (y/n)"
  r <- getLine
  return $ r == "y"

parseArgs :: [String] -> Either String WorkLog
parseArgs [d,i,h] = do
  day <- parseDay d
  hrs <- parseFloat h
  return $ WorkLog day i hrs
parseArgs xs
  | length xs < 3 = throwError "not enough arguments"
  | otherwise     = throwError "too many arguments"

parseDay :: String -> Either String Day
parseDay xs = case parseTimeM False defaultTimeLocale "%Y/%m/%d" xs of
                Just d  -> return d
                Nothing -> throwError $ "cannot parse date " ++ xs ++ " in fomrat yyyy/mm/dd"

parseFloat :: String -> Either String Float
parseFloat xs = case reads xs of
                  [(n,"")] -> return n
                  _        -> throwError "cannot parse hours worked"
