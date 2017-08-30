import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)

main :: IO ()
main = do
  content <- readFile "problem011.txt"
  let l = [ read x :: Int | x <- foldl (++) [] $ map (splitOn " ") (lines content)]
  let rows = getRows l
  let cols = getColums rows
  let maxRows = maximum $ map (maxAdjacent 0) rows
  let maxCols = maximum $ map (maxAdjacent 0) cols
  putStrLn ("maxCols: " ++ show maxCols)
  putStrLn ("maxRows: " ++ show maxRows)

-- NW - SE => [ l !! (20 * x + x) | x <- [0..19]]
-- [ [ l !! ((y+x) * 20 + x) | x <- [0..19-y]] | y <- [16,15..0] ]
-- [ [ l !! ((y+x) + 20 * x) | x <- [0..19-y]] | y <- [1..16] ]
--
-- NE - SW => [ l !! (19 * x) | x <- [1..20]]
--
getNWSE :: [Int] -> [[Int]]
getNWSE xs = [ 
  [ xs !! ((abs y + x) * 20 + x) | x <- [0..19 - abs y], let i = getIndex x y ] | y <- [16,15..(-16)] ]
    where getIndex x y = if (y < 0) then ((abs y + x) + 20 * x) else ((abs y + x) * 20 + x)
 
getColums :: [[Int]] -> [[Int]]
getColums xs = transpose xs

maxAdjacent :: Int -> [Int] -> Int
maxAdjacent m xs = let adjacents = 4
                       s = take adjacents xs
                       p = product s
                   in if (length s < adjacents) then m else maxAdjacent (max m p) (tail xs)

getRows :: [Int] -> [[Int]]
getRows xs = chunksOf 20 xs

-- getRows :: Int -> [Int] -> [[Int]]
-- getRows x xs = fn xs
--   where fn ys = let h = take x ys
--                     t = drop x ys
--                 in if (not $ null t) then h : (fn t) else [h]
