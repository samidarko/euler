import Data.Numbers.Primes (primes)
import Helpers (stringToIntList)
import qualified Data.Set as Set
import Data.List (sort)

pandigitalPrime :: (Int, Int)
pandigitalPrime = fn (0, 0) (reverse $ takeWhile (<=limit) primes)
  where limit = 7654321
        fn (max, nbDigits) (x:xs) = let ys = stringToIntList $ show $ x
                                        sys = Set.fromList ys
                                        lys = length ys
                                        lsys = length sys
                                        isSucc = sort ys == [1..maximum ys]
                                     in if (lys == lsys && lys > nbDigits && isSucc)
                                           then fn (x, lys) xs else fn (max, nbDigits) xs
        fn (max, nbDigits) [] = (max, nbDigits)

