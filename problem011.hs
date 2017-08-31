import Data.List (transpose)
import Data.List.Split (splitOn, chunksOf)

main :: IO ()
main = do
  content <- readFile "problem011.txt"
  let l = [ read x :: Int | x <- foldl (++) [] $ map (splitOn " ") (lines content)]
  let rows = getRows l
  let cols = getColums rows
  let nesw = getNESW l
  let nwse = getNWSE l
  let maxRows = maximum $ map (maxAdjacent 0) rows
  let maxCols = maximum $ map (maxAdjacent 0) cols
  let maxNESW = maximum $ map (maxAdjacent 0) nesw
  let maxNWSE = maximum $ map (maxAdjacent 0) nwse
  putStrLn ("maxCols: " ++ show maxCols)
  putStrLn ("maxRows: " ++ show maxRows)
  putStrLn ("maxNESW: " ++ show maxNESW)
  putStrLn ("maxNWSE: " ++ show maxNWSE)

getNESW :: [Int] -> [[Int]]
getNESW xs = [
  [ xs !! i |  x <- [start y..abs y], let i = getIndex x y ] | y <- [3..19] ++ [(-19)..(-4)] ]
    where getIndex a b = if (b < 0) then (20 * (20 + b) + (19 * a)) else (b + 19 * a)
          start x = if (x < 0) then 1 else 0

getNWSE :: [Int] -> [[Int]]
getNWSE xs = [
  [ xs !! i | x <- [0..19 - abs y], let i = getIndex x y ] | y <- [16,15..(-16)] ]
    where getIndex a b = if (b < 0) then ((abs b + a) + 20 * a) else ((abs b + a) * 20 + a)

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
