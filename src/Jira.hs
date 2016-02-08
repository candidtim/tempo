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
logWork conf ws = do
  request'' <- parseUrl $ issueUrl conf (head ws)
  let request' = applyBasicAuth (getJiraUser conf) (getJiraPassword conf) request''
      (WorkLog d i h) = head ws -- TODO: log all
      params = [("time", pack.show $ h), ("user", (getJiraUser conf)), ("date", datefmt d), ("ansidate", ansidatefmt d)]
      request = urlEncodedBody params request'
  manager  <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager
    return ()
  return ()

datefmt :: Day -> ByteString
datefmt = pack . formatTime defaultTimeLocale "%d/%b/%y"

ansidatefmt :: Day -> ByteString
ansidatefmt = pack . formatTime defaultTimeLocale "%Y-%m-%d"

issueUrl :: Config -> WorkLog -> String
issueUrl conf (WorkLog _ issue _) = printf "https://%s/rest/tempo-rest/1.0/worklogs/%s" (getJiraHost conf) issue
