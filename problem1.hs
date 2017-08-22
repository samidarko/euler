
problem :: Int -> Int
problem x
  | x < 0 = 0
  | otherwise = fn 0 [0..x-1]
  where
    fn a [] = a
    fn a (y:ys)
      | isMultiple = fn (a+y) ys
      | otherwise  = fn a ys
      where
        isMultiple = y `mod` 3 == 0 || y `mod` 5 == 0

-- sum [3,6..999] + sum [5,10..999] - sum [15,30..999] 
-- problem 1000 == 233168
