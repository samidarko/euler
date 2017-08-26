import Data.Char (intToDigit)
-- should stop at 1,000,000 cause total is 10,000,000,000
lexicographicPermutations :: [String]
lexicographicPermutations = 
    [ val | 
      a <- l, 
      b <- l, 
      c <- l, 
      d <- l, 
      e <- l, 
      f <- l, 
--      g <- l, 
--      h <- l, 
--      i <- l, 
--      j <- l, 
--      let val =  a : b : c : d : e : f : g : h : i : j : [], 
      let val =  a : b : c : d : e : f : [], 
          allDifferent val]
  where l = [ intToDigit x | x <- [0 .. 5]]

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

