module Lib
( getGitUser
, getIssuePattern
, getGitLog
, calculateWorkLog
, askToConfirm
, logWork
) where

import System.IO
import System.Process
import Data.Time
import Data.Time.Format
import Text.Regex.Posix
import qualified Data.List as L

import Util


type IssuePattern = String
type User = String
type Issue = String
type HoursWorked = Float

data Commit = Commit Day Issue deriving (Show, Eq, Ord)
data WorkLog = WorkLog Day Issue HoursWorked deriving (Show)


getGitUser :: IO User
getGitUser = fmap trim $ readProcess "git" ["config", "user.email"] []

-- TODO: read ~/.tempo.conf for this
getIssuePattern :: IO IssuePattern
getIssuePattern = return "HB-[0-9]+" -- TODO: return compiled pattern

getGitLog :: User -> [String] -> IssuePattern -> IO [Commit]
getGitLog user args issuePattern = do
  log <- fmap lines $ readProcess "git" (["log", "--format=%aI %s", "--author="++user] ++ args) []
  let relevant = filter (\s -> s =~ issuePattern) log
      issues = map (\s -> s =~ issuePattern) relevant
      dates = map (\s -> readTime defaultTimeLocale "%Y-%m-%d" $ take 10 s) relevant
  return $ zipWith Commit dates issues

calculateWorkLog :: [Commit] -> [WorkLog]
calculateWorkLog gitLog = let grouped = L.groupBy (\(Commit d1 _) (Commit d2 _) -> d1 == d2) $ L.sort gitLog
                              avHours = map ( \cs -> 8.0 / (fromIntegral . length $ cs) ) grouped
                              logs = zipWith ( \cs h -> map (\(Commit d i) -> WorkLog d i h) cs ) grouped avHours
                          in concat logs

askToConfirm :: [WorkLog] -> IO Bool
askToConfirm workLog = do
  putStrLn "Will log following items:"
  mapM_ (putStrLn . show) workLog
  putStrLn "OK? (y/n)"
  r <- getLine
  return $ r == "y"

-- TODO: get JIRA host from config (along with issue pattern apparetnly)
logWork :: [WorkLog] -> IO ()
logWork workLog = do
  -- r <- map (\(WorkLog d i h) -> simpleHttp "https://JIRA.HOST/rest/tempo-rest/1.0/worklogs/"++i ???) workLog
  mapM_ (putStrLn . show) workLog
