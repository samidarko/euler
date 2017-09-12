import Data.Char (digitToInt)

-- l = foldl (++) [] [show x | x <- [1..100]]

-- champernowne i (x:xs)
--   -- | i == 1000000 = (digitToInt x)
--   | i == 10000 = (digitToInt x)
--   | isElem 1     =  (digitToInt x) * helper i xs
--   | otherwise = helper i xs
--   where helper j ys = champernowne (j+1) (ys ++ show (j+1))
--         isElem y = y `elem` [1, 10, 100, 1000]
--         -- isElem y = y `elem` [1, 10, 100, 1000, 10000, 100000]

