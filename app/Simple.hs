module Simple where

import System.Environment
import Control.Monad.Except
import Data.Time
import Data.Time.Format
import Data.Either.Utils

import Config
import Jira
import Git
import CLI


main :: IO ()
main = do
  args <- getArgs
  config <- forceEither <$> readConfig
  let workLog = forceEither $ parseArgs args
  submitLogInteractive config [workLog]

usage = "usage: tempo-simple DATE ISSUE HOURS"

parseArgs :: [String] -> Either String WorkLog
parseArgs [d,i,h] = do
  day <- parseDay d
  hrs <- parseFloat h
  return $ WorkLog day i hrs
parseArgs xs
  | length xs < 3 = throwError $ "not enough arguments; " ++ usage
  | otherwise     = throwError $ "too many arguments; " ++ usage

parseDay :: String -> Either String Day
parseDay xs = case parseTimeM False defaultTimeLocale "%Y/%m/%d" xs of
                Just d  -> return d
                Nothing -> throwError $ "cannot parse date " ++ xs ++ " in fomrat yyyy/mm/dd"

parseFloat :: String -> Either String Float
parseFloat xs = case reads xs of
                  [(n,"")] -> return n
                  _        -> throwError "cannot parse hours worked"
