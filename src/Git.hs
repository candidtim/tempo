module Git
( getGitUser
, getGitLog
, calculateWorkLog
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


getGitLog :: Config -> User -> [String] -> IO [Commit]
getGitLog config user args = do
  let gitArgs = ["log", "--format=%aI %s", "--author="++user] ++ args
  log <- getGitLogLines user gitArgs (getGitRepositories config)
  return $ extractCommits log (getIssuePatterns config)


-- extract commits matching any expected issue patter from git log
extractCommits :: [String] -> [IssuePattern] -> [Commit]
extractCommits log = concatMap (extractCommits' log)

-- extract commits mathcing expected issue pattern from git log
extractCommits' :: [String] -> IssuePattern -> [Commit]
extractCommits' log issuePattern =
  let relevant = filter (=~ issuePattern) log
      issues = map (=~ issuePattern) relevant
      dates = map (parseTimeOrError False defaultTimeLocale "%Y-%m-%d" . take 10) relevant
  in zipWith Commit dates issues


-- extract git log lines for all given repositories, merged to one list
getGitLogLines :: User -> GitArgs -> [FilePath] -> IO [String]
getGitLogLines user gitArgs repos = let listOfLogs = mapM (getGitLogLines' user gitArgs) repos
                                    in  concat <$> listOfLogs

-- extract git log lines for one repository only
getGitLogLines' :: User -> GitArgs -> FilePath -> IO [String]
getGitLogLines' user gitArgs repoPath = do
  let args = ["log", "--format=%aI %s", "--author="++user] ++ gitArgs
      gitProcess = (proc "git" gitArgs){cwd = Just repoPath}
  lines <$> readCreateProcess gitProcess ""


calculateWorkLog :: [Commit] -> [WorkLog]
calculateWorkLog gitLog =
  let noDuplicates = L.nub gitLog
      grouped = L.groupBy (\(Commit d1 _) (Commit d2 _) -> d1 == d2) $ L.sort noDuplicates
      avHours = map (\cs -> 8.0 / (fromIntegral . length $ cs)) grouped
      logs = zipWith (\cs h -> map (\(Commit d i) -> WorkLog d i h) cs) grouped avHours
  in  concat logs
