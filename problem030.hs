import Data.Char (intToDigit)
import Helpers (stringToIntList)

digitFifthPower :: Integer
digitFifthPower = sum [ sp | a <- l,
    let xs = stringToIntList $ show a,
    let sp = sumPower 5 xs,
    show sp == toString xs ]
  where l = [2..355000]

sumPower :: Int -> [Integer] -> Integer
sumPower p xs = sum $ map (^p) xs

toString :: [Integer] -> String
toString xs = [ intToDigit $ fromIntegral x | x <- xs ]

-- digitFifthPower == 443839
