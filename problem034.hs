import qualified Data.Map as M
import Helpers (stringToIntList)

digitFactorials :: Integer -> Integer
digitFactorials y = sum [ x | x <- [3..y], isFactorialSum x ]

factorialMap :: Integer -> Maybe Integer
factorialMap x = M.lookup (fromInteger x) m
    where m = M.fromList $ (0, 1) : [ (x, product [1..x]) | x <- [1..9]]

isFactorialSum :: Integer -> Bool
isFactorialSum x = let l = map factorialMap $ stringToIntList $ show x
                    in case (Just (x==) <*> (sum <$> sequence l)) of
                        Just(y) -> y
                        _ -> False

-- digitFactorials 50000 == 40730
