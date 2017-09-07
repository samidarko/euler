import Helpers (factors)
import qualified Data.Map as M
import Data.Maybe (isJust)

amicableNumbers :: Int
amicableNumbers = sum $ M.keys $ M.filterWithKey testFilter sfm

sumFactors :: Integral t => t -> t
sumFactors 0 = 0
sumFactors x = sum $ factors x

-- sum factors map
sfm = foldl (\acc v -> M.insert v (sumFactors v) acc) M.empty [0..9999]

testFilter :: Int -> Int -> Bool
testFilter k v
  | k == v = False
  | otherwise = fn $ (M.lookup k sfm) >>= (\a -> M.lookup a sfm) >>= (\b -> return (b == k))
  where fn x = case x of
                 Nothing -> False
                 Just(y) -> y

-- amicableNumbers == 31626

