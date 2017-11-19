import Data.List (group)
import Data.Numbers.Primes (primeFactors)

sumPower :: [Int] -> Int
sumPower xs = let b = head xs
               in (1 - b ^ (length xs + 1)) `div` (1 - b)

properDivisorSum x = (product $ map sumPower $ group $ primeFactors x) - x

isAbondunt x = properDivisorSum x > x

abonduntNumbers = sum [ x | x <- [1..limit], x `notElem` absl ]
  where abl = filter isAbondunt [2..limit]
        absl = [ x | i <- [0..length abl - 1], j <- [i..length abl - 1], let x = (abl !! i) + (abl !! j), x <= limit ]
        limit = 28123

