import Data.List (sort)
import qualified Data.Set as Set

permutedMultiples :: [Int] -> Int
permutedMultiples (x:xs) = if (fn == 1) then x else permutedMultiples xs
    where fn = length $ Set.fromList $ map (sort . show . (x*)) [2..6]

-- *Main Prelude> permutedMultiples [2..] == 142857
-- (0.82 secs, 1,321,702,384 bytes)
