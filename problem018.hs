import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "problem018.txt"
  let l = [ [ read x :: Int | x <- xs] | xs <- map (splitOn " ") (lines content) ]
  let paths = sumAdj ((head . head) l) 0 (tail l)
  putStrLn $ show $ maximum $ paths

sumAdj :: Int -> Int -> [[Int]] -> [Int]
sumAdj val pos (x:xs) = sumAdj a pos xs ++ sumAdj b (pos + 1) xs
  where a = val + x !! pos
        b = val + x !! (pos + 1)
sumAdj val _ [] =  [val]

