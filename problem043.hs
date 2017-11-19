import Data.List (permutations)

subStringDivProp :: String -> Bool
subStringDivProp xs = foldl fn True zs
    where 
      fn acc (i, d) = acc && (read (map (xs !!) [i..i+2]) :: Int) `mod` d == 0
      zs = zip [1..] [2, 3, 5, 7, 11, 13, 17]

subStringDivisibility :: Int
subStringDivisibility = sum [ read x :: Int | x <- permutations ['0'..'9'], subStringDivProp x ]

