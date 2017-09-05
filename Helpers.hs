module Helpers (
    stringToIntList
               , fibs
               , primesTo
               , takeEvery
               , factors
               ) where

import Data.Char (digitToInt)

stringToIntList :: String -> [Integer] 
stringToIntList [] = []
stringToIntList (x:xs) = fn x : stringToIntList xs
  where fn = toInteger . digitToInt

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


primesTo xs = eratos xs
    where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])


 -- ordered lists, difference and union
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs


takeEvery n xs = case drop (n-1) xs of
              (y:ys) -> y : takeEvery n ys
              [] -> []

factors y = [ x | x <- [1..y], y `mod` x == 0 ]
