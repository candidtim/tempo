module Worklog
( calculateWorkLog
) where


import qualified Data.List as L

import Config
import Git
import Jira


calculateWorkLog :: Config -> [Commit] -> IO [WorkLog]
calculateWorkLog config gitLog = do
  let noDuplicates = L.nub gitLog
      grouped = L.groupBy (\(Commit d1 _) (Commit d2 _) -> d1 == d2) $ L.sort noDuplicates
  filledIn <- mapM (\(Commit d _ : _) -> getTimeLogged config d) grouped
  let avHours = zipWith (\cs fh -> (8.0-fh) / (fromIntegral . length $ cs)) grouped filledIn
      logs = zipWith (\cs h -> map (\(Commit d i) -> WorkLog d i h) cs) grouped avHours
  return $ concat logs
