import qualified Data.Set as S
import Helpers (stringToIntList)
import Data.Numbers.Primes (primeFactors)

fn :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
fn acc (n, d) = let a = S.fromList $ stringToIntList $ show n
                    b = S.fromList $ stringToIntList $ show d
                    i = S.intersection a b
                    a' = S.difference a i
                    b' = S.difference b i
                    extract = S.elemAt 0
                    isModTen = n `mod` 10 == 0 && d `mod` 10 == 0
                    intDiv x y = fromInteger x / fromInteger y
                 in if n > d || isModTen || S.null i || S.size a' /= 1 || S.size b' /= 1
                       then acc
                       else let n' = extract a'
                                d' = extract b'
                             in if n' > 0 && d' >0 && intDiv n d == intDiv n' d' 
                                   then (n', d'):acc else acc


l = [10..99]

nonTrivial = foldl fn [] [ (n, d) | n <- l, d <- l]
nonTrivialProduct = foldl (\(x1, y1) (x2, y2) -> (x1*x2, y1*y2) ) (1, 1) nonTrivial

-- Found the following on the forum written by AntoineCellerier, an absolute delight
-- product [a Data.Ratio.% b | a <- [1..9], b <- [1..9], c <-[1..9], 10*a+c < 10*c+b, (10*a+c)*b == (10*c+b)*a ]
