import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "problem011.txt"
  let l = [ read x :: Int | x <- foldl (++) [] $ map (splitOn " ") (lines content)]
  let rows = getRows 20 l
  let cols = getColums rows
  let maxRows = maximum $ map (maxAdjacent 0) rows
  let maxCols = maximum $ map (maxAdjacent 0) cols
  putStrLn ("maxCols: " ++ show maxCols)
  putStrLn ("maxRows: " ++ show maxRows)

getRows :: Int -> [Int] -> [[Int]]
getRows x xs = fn xs
  where fn ys = let h = take x ys
                    t = drop x ys
                in if (not $ null t) then h : (fn t) else [h]
 
getColums :: [[Int]] -> [[Int]]
getColums xs = transpose xs

maxAdjacent :: Int -> [Int] -> Int
maxAdjacent m xs = let adjacents = 4
                       s = take adjacents xs
                       p = product s
                   in if (length s < adjacents) then m else maxAdjacent (max m p) (tail xs)

