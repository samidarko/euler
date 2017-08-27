import Data.Numbers.Primes (primes)

summationOfPrimes :: Integer
summationOfPrimes = sum $ takeWhile (<2000000) $ primes

