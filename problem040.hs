import Data.Char (digitToInt)

-- takeWhile ((<3) . length)  [ show x | x <- [0..]]
--
-- l = foldl (++) [] [show x | x <- [1..100]]

-- champernowne i (x:xs)
--   -- | i == 1000000 = (digitToInt x)
--   | i == 10000 = (digitToInt x)
--   | isElem 1     =  (digitToInt x) * helper i xs
--   | otherwise = helper i xs
--   where helper j ys = champernowne (j+1) (ys ++ show (j+1))
--         isElem y = y `elem` [1, 10, 100, 1000]
--         -- isElem y = y `elem` [1, 10, 100, 1000, 10000, 100000]

-- *Main Prelude Set Data.Char Data.Char Char> (999 - 99) * 3
-- 2700
-- *Main Prelude Set Data.Char Data.Char Char> (9999 - 999) * 4
-- 36000
-- *Main Prelude Set Data.Char Data.Char Char> (99999 - 9999) * 5
-- 450000
-- *Main Prelude Set Data.Char Data.Char Char> (999999 - 99999) * 5
-- 4500000
