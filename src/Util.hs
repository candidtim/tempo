module Util
( trim
, parseIsoDate
) where

import Data.Time (Day)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Data.Char (isSpace)
import Data.ByteString.Char8 (ByteString, pack)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseIsoDate :: String -> Day
parseIsoDate xs = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" $ take 10 xs
