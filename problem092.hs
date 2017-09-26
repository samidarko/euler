import Helpers (stringToIntList)
import Control.Monad.State
import qualified Data.Map as M

next x = (sum . map (^2) . stringToIntList . show) x    -- TODO probably the bottle neck

iter :: M.Map Integer Integer -> Integer -> Integer
iter m y
  | y == 89 = 89
  | y == 1 = 1
  | otherwise = case (M.lookup y m) of
      Just(x) -> x
      _ -> iter m (next y)

type Counter = Integer
type SquareDigitState = (M.Map Integer Integer, Counter)

squareDigitChains :: [Integer] -> State SquareDigitState Counter
squareDigitChains [] = do
    (_, counter) <- get
    return counter
squareDigitChains (x:xs) = do
    (mem, counter) <- get
    case (iter mem x) of
        89 -> put (M.insert x 89 mem, counter + 1)
        1 -> put (M.insert x 1 mem, counter)
    squareDigitChains xs

-- *Main> evalState (squareDigitChains [1..10000000]) (M.fromList [], 0)
-- 8581146
-- (150.83 secs, 107,084,852,688 bytes)

