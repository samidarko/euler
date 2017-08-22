

primeFactors :: Int -> [Int]
primeFactors x = fn x [2..x]
  where
    fn x' [] = []
    fn x' (y:ys)
      | x' `mod` y == 0 = y : fn (x' `div` y) [2..x']
      | otherwise = fn x' ys

-- primeFactors 13195 == [5,7,13,29]

maxPrimeFactor :: Int -> Int
maxPrimeFactor x = maximum $ primeFactors x

-- maxPrimeFactor 600851475143 == 6857

