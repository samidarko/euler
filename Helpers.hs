module Helpers (
    stringToIntList
               ) where

import Data.Char (digitToInt)

stringToIntList :: String -> [Integer] 
stringToIntList (x:xs) = fn x : stringToIntList xs
  where fn = toInteger . digitToInt

stringToIntList [] = []
