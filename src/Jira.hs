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
import Data.ByteString.Char8 (ByteString(..), pack)
import qualified Data.Map as Map
import qualified Data.List as L

import Config
import Util


type Issue = String
type HoursWorked = Float

data WorkLog = WorkLog Day Issue HoursWorked

instance Show WorkLog where
  show (WorkLog d i h) = printf "%s - %s - %.1f hours" (show d) i h

data WorkLogJson = WorkLogJson { timeSpentSeconds :: Float
                               , dateStarted      :: String
                               } deriving (Generic)
instance FromJSON WorkLogJson



getTimeLogged :: Config -> Day -> Day -> IO (Map.Map Day HoursWorked)
getTimeLogged conf s e = do
  request'' <- parseUrl $ printf "https://%s/rest/tempo-timesheets/3/worklogs" (getJiraHost conf)
  let request' = applyBasicAuth (getJiraUser conf) (getJiraPassword conf) request''
      params = [("dateFrom", Just $ ansidatefmt s), ("dateTo", Just $ ansidatefmt e)]
      request = setQueryString params request'
  manager <- newManager tlsManagerSettings
  response <- runResourceT $ httpLbs request manager
  let maybeJsonWorklogs = decode (responseBody response) :: Maybe [WorkLogJson]
      jsonWorkLogs  = fromJust maybeJsonWorklogs
      worklogsAsPairs = map (\(WorkLogJson s d) -> (parseIsoDate d, s/3600.0)) jsonWorkLogs
      pairsByDate = L.groupBy ((==) `on` fst) worklogsAsPairs
      worklogsByDate = foldl (\m ws@((d,_):_) -> Map.insert d (map snd ws) m) Map.empty pairsByDate
  return $ Map.map sum worklogsByDate


logWork :: Config -> [WorkLog] -> IO ()
logWork conf ws = mapM_ (logOne conf) ws

logOne :: Config -> WorkLog -> IO ()
logOne conf (WorkLog d i h) = do
  request'' <- parseUrl $ issueUrl (getJiraHost conf) i
  let request' = applyBasicAuth (getJiraUser conf) (getJiraPassword conf) request''
      params = [("time", pack.show $ h), ("user", getJiraUser conf), ("date", datefmt d), ("ansidate", ansidatefmt d)]
      request = urlEncodedBody params request'
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    return ()

datefmt :: Day -> ByteString
datefmt = pack . formatTime defaultTimeLocale "%d/%b/%y"

ansidatefmt :: Day -> ByteString
ansidatefmt = pack . formatTime defaultTimeLocale "%Y-%m-%d"

issueUrl :: String -> Issue -> String
issueUrl host issue = printf "https://%s/rest/tempo-rest/1.0/worklogs/%s" host issue
