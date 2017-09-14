import Data.Char (digitToInt)

data Item = Item { index :: Int, offset :: Int, digits :: Int, start :: Int } deriving Show

getItems = map fn indexes
  where offsets = scanl1 (+) (10 : [ x * 9 * 10^(x-1) | x <- [2..6]])
        indexes = [ 10^p | p <- [1..6] ]
        fn x = let od = last $ takeWhile ((x>=) . fst) (offsets `zip` [2..]) -- find offset and digit
                   offset = fst od
                   digits = snd od
                   start = 10^(digits-1)
                in Item { index = x, offset = offset, digits = digits, start = start }

-- items = [
--         Item 0 0 1 1, -- need to cheat for the index due to multiplication
--         Item 10 10 2 10,
--         Item 100 10 2 10,
--         Item 1000 190 3 100,
--         Item 10000 2890 4 1000,
--         Item 100000 38890 5 10000,
--         Item 1000000 488890 6 100000
--     ]

extract :: Item -> Int
extract x = let s = start x
                o = offset x
                i = index x
                d = digits x
                p = (i - o) `div` d -- position
                n = [s .. s * 10 - 1] !! p
            in digitToInt $ show n !! (i - o - p * d)

champernowne :: Int
champernowne = product $ map extract (Item 0 0 1 1 : getItems)
-- champernowne = product $ map extract items

-- champernowne == 210

-- a brute force solution found euler forum T.T
-- import Data.Char
-- generateNumber x = (show x) ++ (generateNumber (x+1))
-- number = generateNumber 0
-- --d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
-- dList = [1, 10, 100, 1000, 10000, 100000, 1000000]
-- result = product [digitToInt (number!!x) | x<-dList]
