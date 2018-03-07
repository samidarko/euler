import qualified Data.Set as S
import Helpers (stringToIntList)

l = [10..99]
-- l = [ (n, d) | n <- l, d <- l, n < d, n `mod` 10 /= 0 && d `mod` 10 /= 0 ]

-- a = S.fromList [4, 9]
-- b = S.fromList [9, 8]
-- Prelude.foldl fn [] [ (n, d) | n <- l, d <- l]
-- S.fromList $ Prelude.foldl fn [] [ (n, d) | n <- l, d <- l]

-- fn :: (Show a, Integral a) => [(Integer, Integer)] -> (a, a) -> [(Integer, Integer)]
fn acc (n, d) = let a = S.fromList $ stringToIntList $ show n
                    b = S.fromList $ stringToIntList $ show d
                    i = S.intersection a b
                    a' = S.difference a i
                    b' = S.difference b i
                    extract = S.elemAt 0
                    isModTen = n `mod` 10 == 0 && d `mod` 10 == 0
                 in if n > d || isModTen || S.null i || S.size a' /= 1 || S.size b' /= 1
                       then acc
                       else let n' = extract a'
                                d' = extract b'
                             in if n' > 0 && d' >0 && n `div` d == n' `div` d' 
                                   then (n, d, n', d'):acc else acc

-- (extract a', extract b'):acc

main :: IO ()
main = putStrLn "hello"
