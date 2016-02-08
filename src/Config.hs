module Config
( Config(..)
, IssuePattern
, readConfig
) where

import System.Directory
import System.FilePath
import Data.List.Split
import Data.ConfigFile
import Data.Either.Utils
import Control.Monad.Except
import Data.ByteString.Char8 (ByteString(..), pack)
import Data.ByteString.Base64 (decodeLenient)


type IssuePattern = String

data Config = Config { getGitRepositories::[FilePath]
                     , getIssuePatterns::[IssuePattern]
                     , getJiraHost::String
                     , getJiraUser::ByteString
                     , getJiraPassword::ByteString
                     } deriving (Show)


readConfig :: IO Config
readConfig = do
  homePath <- getHomeDirectory
  errorOrConfig <- runExceptT $ do
    cp       <- join $ liftIO $ readfile emptyCP (homePath </> ".tempo.conf")
    repos    <- get cp "git" "repos"
    projects <- get cp "jira" "projects"
    jiraHost <- get cp "jira" "host"
    jiraUser <- get cp "jira" "user"
    jiraPass <- get cp "jira" "pass"
    return $ Config (splitOn "," $ repos) (splitOn "," $ projects) jiraHost (pack jiraUser) (decodeLenient . pack $ jiraPass)
  return $ forceEither errorOrConfig -- TODO: do not forceEither
