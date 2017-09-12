import qualified Data.Set as Set

distinctPowers :: Int -> Int
distinctPowers n = let l = [2..n]
                    in length $ Set.fromList  [ a^b | a <- l, b <- l]

-- distinctPowers 100 == 9183
