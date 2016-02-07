module Git
( getGitUser
, getGitLog
, calculateWorkLog
) where

import System.IO
import System.Process
import Data.Time.Format
import Text.Regex.Posix
import Data.Time
import qualified Data.List as L

import Config
import Util
import Jira


type User = String

data Commit = Commit Day Issue deriving (Show, Eq, Ord)


getGitUser :: IO User
getGitUser = fmap trim $ readProcess "git" ["config", "user.email"] []

getGitLog :: Config -> User -> [String] -> IO [Commit]
getGitLog config user args = do
  log <- fmap lines $ readProcess "git" (["log", "--format=%aI %s", "--author="++user] ++ args) []
  let issuePattern = head . getIssuePatterns $ config -- TODO: support multiple patterns
      relevant = filter (\s -> s =~ issuePattern) log
      issues = map (\s -> s =~ issuePattern) relevant
      dates = map (\s -> parseTimeOrError False defaultTimeLocale "%Y-%m-%d" $ take 10 s) relevant
  return $ zipWith Commit dates issues

calculateWorkLog :: [Commit] -> [WorkLog]
calculateWorkLog gitLog = let grouped = L.groupBy (\(Commit d1 _) (Commit d2 _) -> d1 == d2) $ L.sort gitLog
                              avHours = map ( \cs -> 8.0 / (fromIntegral . length $ cs) ) grouped
                              logs = zipWith ( \cs h -> map (\(Commit d i) -> WorkLog d i h) cs ) grouped avHours
                          in concat logs
