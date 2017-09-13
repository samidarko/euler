import Data.Char (digitToInt)

nbOfDigits = [1..7]

indices = [1, 10, 100, 1000, 10000, 100000, 1000000]
digitsBySeq = 10 : [ nbOfDigits !! i * indices !! i * 9 * 10^(i-1)
  | i <- [1..6] ] 

digitsAcc = scanl1 (+) digitsBySeq

extract i = let x = indices !! i
             in if (x < 1000) then untilOneHundred x else aboveOneHundred i

untilOneHundred x = digitToInt $ foldl (++) [] [show x | x <- [0..55]] !! x

aboveOneHundred i = let x = indices !! i
                        s = last $ takeWhile (<x) digitsAcc
                        offset = x - s
                        pos = offset `div` (nbOfDigits !! i)
                        value = [x .. x * 10 - 1] !! pos
                     in digitToInt $ show value !! (offset `mod` i)

-- map extract [0 .. length indices - 1] == [1,1,5,1,9,1,1]
-- l !! 1000 == 3
-- l !! 10000 == 7
-- l !! 100000 == 2
-- l !! 1000000 == ? => by deduction == 1

-- champernowne = 210
