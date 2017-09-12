import Data.Char (digitToInt)

nbOfDigits = [1..7]
indices = [1, 10, 100, 1000, 10000, 100000, 1000000]
digitsBySeq = 10 : [ nbOfDigits !! i * indices !! i * 9 * 10^(i-1)
  | i <- [1..6] ] 

extract i = let x = indices !! i
             in if (x < 1000) then
                              digitToInt $ foldl (++) [] [show x | x <- [0..100]] !! x
                              else another i

another i = let s = sum $ take (i-1) digitsBySeq
                x = indices !! i
                offset = x - s
                pos = offset `div` (nbOfDigits !! (i-1))
                value = [indices !! (i-1) .. indices !! i-1] !! pos
             in digitToInt $ show value !! (offset `mod` i)


-- 1000000-3889
-- 10 * 1 + 90 * 2 + 900 * 3
-- 10000 - 2890
-- 10 * 1 + 90 * 2 + 900 * 3 + 9000 * 4
-- [1000..9999] !! 1777 
-- 1777*4
-- 7110 - 1777*4
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
-- 1 * 1 * 5 * 3 * 7
-- 10 * 1 + 90 * 2 + 900 * 3 + 9000 * 4
-- 38890

