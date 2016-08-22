module Worklog
( calculateWorkLog
) where

import Data.Time
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Function
import Data.Maybe

import Config
import Git
import Jira


calculateWorkLog :: Config -> [Commit] -> IO [WorkLog]
calculateWorkLog _ [] = return []
calculateWorkLog config log = do
  let grouped' = groupCommits (L.nub log)
      dates = L.sort $ Map.keys grouped'
  timeSheet <- getTimeLogged config (head dates) (last dates)
  let workLogs = concatMap (\d -> dayWorklog d (fromJust $ Map.lookup d grouped') timeSheet) dates
  return $ filter (\(WorkLog _ _ h) -> h > 0) workLogs

groupCommits :: [Commit] -> Map.Map Day [Commit]
groupCommits = foldl (\m c@(Commit d _) -> appendByDay d c m) Map.empty
  where appendByDay d c m = Map.insert d (c:commitsByDay d m) m
        commitsByDay  d m = fromMaybe [] (Map.lookup d m)

dayWorklog :: Day -> [Commit] -> Map.Map Day HoursWorked -> [WorkLog]
dayWorklog d cs tsh =
  let hoursFilled = fromMaybe 0.0 $ Map.lookup d tsh
      hoursLeft   = 8.0 - hoursFilled
      hoursPerItm = hoursLeft / (fromIntegral . length $ cs)
  in  map (\(Commit _ i) -> WorkLog d i hoursPerItm) cs
