import Data.Numbers.Primes (isPrime, primes)

rotations :: Integer -> [Integer]
rotations x = fn (length s - 1) s
  where s = show x
        fn 0 (y:ys) = (toi $ ys ++ [y]) : []
        fn r (y:ys) = let n = ys ++ [y]
                       in toi n : fn (r-1) n
        toi i = read i :: Integer 

circularPrimes :: Int
circularPrimes = length $ filter f $ takeWhile (<1000000) primes
  where f = all isPrime . rotations

-- circularPrimes == 55
