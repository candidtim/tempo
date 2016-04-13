module Worklog
( calculateWorkLog
) where


import qualified Data.List as L
import qualified Data.Map as Map
import Data.Function
import Data.Maybe

import Config
import Git
import Jira


calculateWorkLog :: Config -> [Commit] -> IO [WorkLog]
calculateWorkLog config gitLog = do
  let noDuplicates = L.nub gitLog
      grouped = L.groupBy (\(Commit d1 _) (Commit d2 _) -> d1 == d2) $ L.sort noDuplicates
      sorted  = L.sortBy (compare `on` \(Commit d _ : _) -> d) grouped
      startDay = (\(Commit d _) -> d) $ head.head $ sorted
      endDay = (\(Commit d _) -> d) $ head.last $ sorted
  timeLogged <- getTimeLogged config startDay endDay
  let filledInMaybe = map (\(Commit d _ : _) -> Map.lookup d timeLogged) sorted
      filledIn = map (fromMaybe 0.0) filledInMaybe
      avHours = zipWith (\cs fh -> (8.0-fh) / (fromIntegral . length $ cs)) grouped filledIn
      logs = zipWith (\cs h -> map (\(Commit d i) -> WorkLog d i h) cs) grouped avHours
  return $ concat logs
