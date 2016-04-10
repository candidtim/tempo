{-# LANGUAGE OverloadedStrings #-}

module Jira
( Issue
, HoursWorked
, WorkLog(..)
, logWork
) where

import Data.Time
import Data.Time.Format
import Text.Printf
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 (ByteString(..), pack)

import Config


type Issue = String
type HoursWorked = Float

data WorkLog = WorkLog Day Issue HoursWorked

instance Show WorkLog where
  show (WorkLog d i h) = printf "%s - %s - %.1f hours" (show d) i h


logWork :: Config -> [WorkLog] -> IO ()
logWork conf ws = mapM_ (logOne conf) ws

logOne :: Config -> WorkLog -> IO ()
logOne conf (WorkLog d i h) = do
  request'' <- parseUrl $ issueUrl conf i
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

issueUrl :: Config -> Issue -> String
issueUrl conf issue = printf "https://%s/rest/tempo-rest/1.0/worklogs/%s" (getJiraHost conf) issue
