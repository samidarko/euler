import Control.Monad.State

type CollatzValue = Int
type CollatzLength = Int
type CollatzState = (CollatzLength, CollatzValue)

longestCollatzSequence :: Int -> State CollatzState (Int, Int)
longestCollatzSequence 1 = do
    (l, v) <- get
    return (l, v)
longestCollatzSequence x = do
    let cl = length $ collatzSequence x
    (collatzLength, collatzValue) <- get
    if (cl > collatzLength) 
       then put (cl, x) 
       else put (collatzLength, collatzValue)
    longestCollatzSequence (x-1)


collatzSequence :: Int -> [Int]
collatzSequence x = fn x
  where
    fn n
      | n == 1 = []
      | odd n = let n' = (3 * n + 1) in n' : fn n'
      | even n = let n' = (n `div` 2) in n' : fn n'

-- evalState (longestCollatzSequence 1000000) (0, 0) == (524,837799)
