{-# LANGUAGE OverloadedStrings #-}

module Jira
( Issue
, HoursWorked
, WorkLog(..)
, getTimeLogged
, logWork
) where

import Data.Time
import Data.Time.Format
import Text.Printf
import Network.HTTP.Conduit
import Data.Aeson
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 (ByteString(..), pack)

import Config


type Issue = String
type HoursWorked = Float

data WorkLog = WorkLog Day Issue HoursWorked

instance Show WorkLog where
  show (WorkLog d i h) = printf "%s - %s - %.1f hours" (show d) i h


getTimeLogged :: Config -> Day -> IO HoursWorked
getTimeLogged conf d = do
  request'' <- parseUrl $ printf "https://%s/rest/tempo-timesheets/3/worklogs" (getJiraHost conf)
  let request' = applyBasicAuth (getJiraUser conf) (getJiraPassword conf) request''
      params = [("dateFrom", Just $ ansidatefmt d), ("dateTo", Just $ ansidatefmt d)]
      request = setQueryString params request'
  resp <- withManager $ \m -> httpLbs request m
  let rs = responseBody resp
  return $ case decode rs of
    Nothing -> 0
    Just (Object o) -> 8
    _ -> 8


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
