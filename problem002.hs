import Helpers (fibs)


evenFibNumbs = sum $ filter even $ takeWhile (<=4000000) fibs 
-- evenFibNumbs == 4613732
