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
import Data.List.Split

import Config
import Util
import Jira


type Sha1 = String
type Message = String
type User = String
type GitArgs = [String]

data Commit = Commit Sha1 Day Message deriving (Show, Eq)
data RelevantCommit = RelevantCommit Day Issue deriving (Show, Eq, Ord)


getGitUser :: IO User
getGitUser = trim <$> readProcess "git" ["config", "user.email"] []


getGitLog :: Config -> User -> [String] -> IO [RelevantCommit]
getGitLog config user args = concat <$> mapM (getGitLog' config user args) (getGitRepositories config)

getGitLog' :: Config -> User -> [String] -> FilePath -> IO [RelevantCommit]
getGitLog' config user args repo = do
  log <- gitLog' user args repo
  let issuePatterns = getIssuePatterns config
      byMessages = fromMessages log issuePatterns
  byBranches <- fromBranches log repo issuePatterns
  return $ byMessages ++ byBranches

fromBranches :: [Commit] -> FilePath -> [IssuePattern] -> IO [RelevantCommit]
fromBranches log repo issuePatterns =
  concat <$> mapM (fromBranches' repo issuePatterns) log





--- TODO: finally it looks like I took a wrong approach; if all done locally -
--  can reliably know a branch name and a message for any commit, from reflog
--  drawback - only locally (is it?)
--  see stackoverflow.com/questions/2706797/finding-what-branch-a-git-commit-came-from
--  second idea in first answer




fromBranches' :: FilePath -> [IssuePattern] -> Commit -> IO [RelevantCommit]
fromBranches' repo issuePatterns (Commit sha1 day _) = do
  let args = ["branch", "-a", "--contains="++sha1]
      gitProcess = (proc "git" args){cwd = Just repo}
  allBranches <- map (drop 2) . lines <$> readCreateProcess gitProcess ""
  let onlyNames = map (last . splitOn "/") allBranches
      branches = L.nub onlyNames
  case branches of
    [] -> return []
    _ -> do
           branch <- earliestBranch branches
           return $ map (RelevantCommit day) $ findIssues branch issuePatterns

earliestBranch :: [String] -> IO String
earliestBranch bs = do
  allBranches <- allBranchesByCreation
  return $ head $ dropWhile (`elem` bs) allBranches

allBranchesByCreation :: IO [String]
allBranchesByCreation = return [] -- TODO: list all branches in chronological order

-- Note: doesn't attempt to match same pattern several times (TODO: match multiple times?)
findIssues :: String -> [IssuePattern] -> [Issue]
findIssues xs issuePatterns = filter (not . null) $ map (xs =~) issuePatterns

-- extract commits matching any expected issue patter from git log
fromMessages :: [Commit] -> [IssuePattern] -> [RelevantCommit]
fromMessages log issuePatterns =
  concatMap (\(Commit _ d m) -> map (RelevantCommit d) (findIssues m issuePatterns)) log


-- extract git log lines for a repository
gitLog' :: User -> GitArgs -> FilePath -> IO [Commit]
gitLog' user gitArgs repoPath = do
  let args = ["log", "--format=%H%ai%s", "--author="++user] ++ gitArgs
      gitProcess = (proc "git" args){cwd = Just repoPath}
  logLines <- lines <$> readCreateProcess gitProcess ""
  return $ map parseLogLine logLines

-- parses log lines from `git log` in fomrat `%H%ai%s` into a `Commit`
-- TODO: splitOn
parseLogLine :: String -> Commit
parseLogLine s =
  let sha1   = take 40 s -- sha1 == 40ch
      dayStr = take 10 (drop 40 s) -- date == 10ch after sha1
      day    = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" dayStr
      msg    = drop 65 s -- message == rest after sha1+time(25ch)
  in  Commit sha1 day msg


calculateWorkLog :: [RelevantCommit] -> [WorkLog]
calculateWorkLog log =
  let noDuplicates = L.nub log
      grouped = L.groupBy (\(RelevantCommit d1 _) (RelevantCommit d2 _) -> d1 == d2) $ L.sort noDuplicates
      avHours = map (\cs -> 8.0 / (fromIntegral . length $ cs)) grouped
      logs = zipWith (\cs h -> map (\(RelevantCommit d i) -> WorkLog d i h) cs) grouped avHours
  in  concat logs
