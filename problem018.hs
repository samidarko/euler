import Data.List.Split (splitOn)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

main :: IO ()
main = do
  content <- readFile "problem018.txt"
  let l = [ [ read x :: Int | x <- xs] | xs <- map (splitOn " ") (lines content) ]
  putStrLn $ show l

-- dfs :: [[Int]] -> Int -> Int -> Int
-- dfs tree depth pos
--   | depth > 0 =

