module Helpers (
    stringToIntList
               , fibs
               ) where

import Data.Char (digitToInt)

stringToIntList :: String -> [Integer] 
stringToIntList [] = []
stringToIntList (x:xs) = fn x : stringToIntList xs
  where fn = toInteger . digitToInt

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
