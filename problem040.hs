import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Maybe (fromJust)

data Item = Item { index :: Int, offset :: Int, digits :: Int, start :: Int } deriving Show

items = [
        Item 0 0 1 1, -- need to cheat for the index due to multiplication
        Item 10 10 2 10,
        Item 100 10 2 10,
        Item 1000 190 3 100,
        Item 10000 2890 4 1000,
        Item 100000 38890 5 10000,
        Item 1000000 488890 6 100000
    ]

extract :: Item -> Int
extract x = let s = start x
                o = offset x
                i = index x
                d = digits x
                p = (i - o) `div` d -- position
                n = [s .. s * 10 - 1] !! p
            in digitToInt $ show n !! (i - o - p * d)

champernowne :: Int
champernowne = product $ map extract items

-- champernowne == 210
