module Git
( Commit(..)
, getGitUser
, getGitLog
) where

import System.IO
import System.Process
import System.FilePath

import Data.Time.Format
import Text.Regex.Posix
import Data.Time
import qualified Data.List as L

import Config
import Util
import Jira


type User = String
type GitArgs = [String]

data Commit = Commit Day Issue deriving (Show, Eq, Ord)


getGitUser :: IO User
getGitUser = trim <$> readProcess "git" ["config", "user.email"] []


getGitLog :: Config -> User -> GitArgs -> IO [Commit]
getGitLog config user args = do
  log <- reflog user args (getGitRepositories config)
  return $ extractCommits log (getIssuePatterns config)


extractCommits :: [String] -> [IssuePattern] -> [Commit]
extractCommits log ps = concatMap (`extractCommits'` ps) log

extractCommits' :: String -> [IssuePattern] -> [Commit]
extractCommits' msg ps =
  let issues = concatMap (\p -> getAllTextMatches (msg =~ p)) ps
      date   = parseIsoDate msg
  in  map (Commit date) issues


-- extract git log lines for all given repositories, merged to one list
reflog :: User -> GitArgs -> [FilePath] -> IO [String]
reflog user gitArgs repos =
  let listOfLogs = mapM (reflog' user gitArgs) repos
  in  concat <$> listOfLogs

-- extract git log lines for one repository only
reflog' :: User -> GitArgs -> FilePath -> IO [String]
reflog' user gitArgs repoPath = do
  let args = ["log", "-g", "--all", "--format=%ai %gD %gs", "--author="++user] ++ gitArgs
      gitProcess = (proc "git" args){cwd = Just repoPath}
  lines <$> readCreateProcess gitProcess ""
