import Data.List (sortBy)

largestPalindrome :: Int -> Int
largestPalindrome x = 
    let l = [x,x-1..0]
        product = sortBy (flip compare) [i * j | i <- l, j <- l]
        fn (y:ys)
          | show y == (reverse . show) y = y
          | otherwise = fn ys
        in fn product

-- largestPalindrome 99 == 9009
-- largestPalindrome 999 == 906609
