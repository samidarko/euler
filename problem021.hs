import Helpers (factors)
import qualified Data.Map as M
import Data.Maybe (isJust)

-- sum $ init $ factors 284 == 220
-- sum $ init $ factors 220 == 284

-- amicableNumbers :: M.Map k a -> [Int] -> Int
-- amicableNumbers m (x:xs) = sum $ M.keys $ M.filterWithKey testFilter temp
-- amicableNumbers :: Int
-- amicableNumbers = sum $ M.keys $ M.filterWithKey testFilter temp
amicableNumbers = M.filterWithKey testFilter temp

sumFactors :: Integral t => t -> t
sumFactors 0 = 0
sumFactors x = sum $ init $ factors x

-- temp = foldl (\acc v -> M.insert v (sumFactors v) acc) M.empty [0..9999]
temp = foldl (\acc v -> M.insert v (sumFactors v) acc) M.empty [0..3000]
-- let x = 220
-- (M.lookup x temp) >>= (\a -> M.lookup a temp) >>= (\b -> return (b == x))
-- isJust $ (M.lookup x temp) >>= (\a -> M.lookup a temp) >>= (\b -> return (b == x))

testFilter :: Int -> Int -> Bool
testFilter k _ = fn $ (M.lookup k temp) >>= (\a -> M.lookup a temp) >>= (\b -> return (b == k))
    where fn x = case x of
                   Nothing -> False
                   Just(y) -> y
