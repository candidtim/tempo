module MakeConfig where

import System.Directory
import System.FilePath
import Text.Printf
import System.Console.Haskeline
import Control.Monad
import Data.ByteString.Char8 (ByteString(..), pack, unpack)
import Data.ByteString.Base64 (encode)
import Data.List (intercalate)

import Config
import Jira


main :: IO ()
main = do
  putStrLn "This tool will guide you through configuration\n\
           \process necessary for `tempo-git` to work.\n"
  projects <- queryProjects
  host <- queryHost
  user <- queryUser
  pass <- queryPass
  isValid <- isValidJiraConfig projects host user pass
  when isValid $ do
    repos <- queryGitRepos
    writeConfig repos projects host user pass


queryProjects :: IO [String]
queryProjects = do
  putStrLn "Enter codes of JIRA projects to log time to.\n\
           \JIRA project code is the part of tickets name before first dash.\n\
           \Enter empty input to pass to next step.\n"
  queryProjects'

queryProjects' :: IO [String]
queryProjects' = do
  putStrLn "Enter code of JIRA project: "
  project <- getLine
  case project of
    "" -> return []
    _  -> do
      moreProjects <- queryProjects'
      return $ project:moreProjects


queryHost :: IO String
queryHost = do
  putStrLn "Enter JIRA host name or IP.\n\
           \E.g.: myjira.atlassian.net"
  getLine

queryUser :: IO String
queryUser = do
  putStrLn "\nREAD CAREFULLY BEFORE PROCEEDING FURTHER\n\
           \\n\
           \You will be further prompted to enter JIRA user name and password.\n\
           \If you do not use SSO (like login to JIRA with a Google account, for\n\
           \example), you can proceed now. If you use SSO, note that user name\n\
           \and password should NOT be of your SSO account, but those of JIRA\n\
           \itself. If you never created or used JIRA's proper account, you can\n\
           \do so via JIRA login form by requesting a password reset: go to JIRA,\n\
           \sign out, open sign-in form and go via 'Forgot a password' option\n\
           \indicating your user name (not e-mail). If you don't know your user\n\
           \name, go via `Forgot a user name` option first. Resetting this password\n\
           \will not impact your SSO behaviour. Once you reset a password, proceed\n\
           \further here.\n"
  putStrLn "Enter your JIRA user name:"
  getLine

queryPass :: IO String
queryPass = do
  putStrLn "\nEtner your JIRA password.\n\
           \Note, your password will be stored in the config file in the\n\
           \base64-encoded form. It is not ciphered, just not plaintext.\n"
  queryPass'

queryPass' :: IO String
queryPass' = do
  pass <- runInputT defaultSettings (getPassword (Just '*') "Password: ")
  case pass of
    Just p  -> return p
    Nothing -> queryPass'

isValidJiraConfig :: [String] -> String -> String -> String -> IO Bool
isValidJiraConfig projects host user pass = do
  -- TODO: request JIRA API to verify list of deifned projects; catch errors if any
  return True


queryGitRepos :: IO [FilePath]
queryGitRepos = do
  putStrLn "Enter paths to Git repositires to scan.\n\
           \Paths must be absolute.\n\
           \Enter empty input to pass to next step.\n"
  queryGitRepos'

queryGitRepos' :: IO [FilePath]
queryGitRepos' = do
  maybeRepo <- queryGitRepo
  case maybeRepo of
    Nothing -> return []
    Just r  -> do
      moreRepos <- queryGitRepos'
      return $ r:moreRepos

queryGitRepo :: IO (Maybe FilePath)
queryGitRepo = do
  putStrLn "Enter path to Git repository: "
  repoPath <- getLine
  dotGitExists <- doesDirectoryExist $ repoPath </> ".git"
  case repoPath of
    ""                   -> return Nothing
    _ | dotGitExists     -> do putStrLn "OK\n"
                               return $ Just repoPath
    _ | not dotGitExists -> do putStrLn "This directory doesn't exist or is not a valid Git repository\n"
                               queryGitRepo


writeConfig :: [String] -> [String] -> String -> String -> String -> IO ()
writeConfig repos projects host user pass = do
  putStrLn "Put following content into `~/.tempo.conf` file:"
  putStrLn ""
  putStrLn "[git]"
  putStrLn $ printf "repos = %s" (intercalate "," repos)
  putStrLn ""
  putStrLn "[jira]"
  putStrLn $ printf "projects = %s" (intercalate "," projects)
  putStrLn $ printf "host = %s" host
  putStrLn $ printf "user = %s" user
  putStrLn $ printf "pass = %s" (unpack (encode . pack $ pass))
