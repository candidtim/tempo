{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Jira
( Issue
, HoursWorked
, WorkLog(..)
, getTimeLogged
, logWork
) where


import GHC.Generics

import Data.Function (on)
import Data.Maybe
import Data.Time
import Data.Time.Format
import Text.Printf
import Network.HTTP.Conduit
import Data.Aeson
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 (ByteString(..), pack, unpack)
import qualified Data.Map as Map
import qualified Data.List as L

import Config
import Util


type Issue = String
type HoursWorked = Float

data WorkLog = WorkLog Day Issue HoursWorked

instance Show WorkLog where
  show (WorkLog d i h) = printf "%s - %s - %.1f hours" (show d) i h


data IssueJson = IssueJson { key :: String } deriving Generic
instance FromJSON IssueJson
instance ToJSON IssueJson

data AuthorJson = AuthorJson { name :: String } deriving Generic
instance FromJSON AuthorJson
instance ToJSON AuthorJson

data WorkLogJson = WorkLogJson { issue            :: IssueJson
                               , dateStarted      :: String
                               , timeSpentSeconds :: Float
                               , author           :: AuthorJson
                               } deriving Generic
instance FromJSON WorkLogJson
instance ToJSON WorkLogJson


worklogsUrl :: Config -> IO Request
worklogsUrl conf = parseUrl $ printf "https://%s/rest/tempo-timesheets/3/worklogs" (getJiraHost conf)


getTimeLogged :: Config -> Day -> Day -> IO (Map.Map Day HoursWorked)
getTimeLogged conf s e = do
  request'' <- worklogsUrl conf
  let request' = applyBasicAuth (getJiraUser conf) (getJiraPassword conf) request''
      params = [("dateFrom", Just $ datefmtb s), ("dateTo", Just $ datefmtb e)]
      request = setQueryString params request'
  manager <- newManager tlsManagerSettings
  response <- runResourceT $ httpLbs request manager
  let maybeJsonWorklogs = decode (responseBody response) :: Maybe [WorkLogJson]
      jsonWorkLogs  = fromJust maybeJsonWorklogs
      worklogsAsPairs = map (\(WorkLogJson _ d t _) -> (parseIsoDate d, t/3600.0)) jsonWorkLogs
      pairsByDate = L.groupBy ((==) `on` fst) worklogsAsPairs
      worklogsByDate = foldl (\m ws@((d,_):_) -> Map.insert d (map snd ws) m) Map.empty pairsByDate
  return $ Map.map sum worklogsByDate


logWork :: Config -> [WorkLog] -> IO ()
logWork conf ws = mapM_ (logOne conf) ws


logOne :: Config -> WorkLog -> IO ()
logOne conf (WorkLog d i h) = do
  request'' <- worklogsUrl conf
  let request' = applyBasicAuth (getJiraUser conf) (getJiraPassword conf) request''
      json = WorkLogJson (IssueJson i) (datefmt d) (h*3600.0) (AuthorJson $ unpack (getJiraUser conf))
      request = request'
                  { method = "POST"
                  , requestBody = (RequestBodyLBS (encode json))
                  }
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    return ()


datefmt :: Day -> String
datefmt = formatTime defaultTimeLocale "%Y-%m-%d"

datefmtb :: Day -> ByteString
datefmtb = pack . datefmt
