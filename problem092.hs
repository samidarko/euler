import Helpers (stringToIntList)
import Control.Monad.State
import qualified Data.Map as M

next x = (sum . map (^2) . stringToIntList . show) x

iter y
  | y == 89 = 89
  | y == 1 = 1
  | otherwise = iter $ next y

chain x = case (iter x) of 
            89 -> Right x
            1 -> Left x

rightCounter e = case e of Right _ -> 1; _ -> 0

type Counter = Int
type SquareDigitState = (M.Map Int Int, Counter)

squareDigitChains :: [Int] -> State SquareDigitState Counter
squareDigitChains [] = do
    (_, counter) <- get
    return counter
squareDigitChains (x:xs) = do

-- squareDigitChains = foldl (\acc v -> acc + rightCounter v) 0 (map chain [1..100000])

