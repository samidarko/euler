
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- How many different ways can £2 be made using any number of coins?
-- https://rosettacode.org/wiki/Count_the_coins#Haskell
--
coinSums :: Integer
coinSums = count 200 [1, 2, 5, 10, 20, 50, 100, 200]

count :: (Integral a) => a -> [a] -> a
count 0 _ = 1
count _ [] = 0
count x (c:coins) =
  sum
    [ count (x - (n * c)) coins
    | n <- [0 .. (quot x c)] ]
 
-- count :: Integral a => [Int] -> [a]
-- count = foldr addCoin (1 : repeat 0)
--   where
--     addCoin c oldlist = newlist
--       where
--         newlist = take c oldlist ++ zipWith (+) newlist (drop c oldlist)

