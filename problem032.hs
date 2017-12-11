import Data.List (permutations)
import qualified Data.Set as Set

data Value = Value { multiplicand :: Int, multiplier :: Int, prod :: Int } deriving (Show, Eq)

p = permutations [1..9]

toInt :: [Int] -> Int
toInt = foldl1 (\acc v -> 10 * acc + v)

getValues :: [Int] -> [Value]
getValues xs = filter (\v -> multiplier v * multiplicand v == prod v) [
  Value (toInt $ take 1 xs) (toInt $ take 4 $ drop 1 xs) (toInt $ drop 5 xs),
  Value (toInt $ take 2 xs) (toInt $ take 3 $ drop 2 xs) (toInt $ drop 5 xs)
        ]

allValues :: [Value]
allValues = foldl (\acc v -> acc ++ getValues v) [] p

pandigitalProducts :: Int
pandigitalProducts = sum $ Set.fromList $ map prod allValues

