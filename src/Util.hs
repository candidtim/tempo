module Util
( trim
) where

import Data.Char (isSpace)
import Data.ByteString.Char8 (ByteString, pack)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
