

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

evenFibNumbs = sum $ filter even $ takeWhile (<=4000000) fibs 
-- evenFibNumbs == 4613732
