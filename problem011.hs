import Data.List.Split (splitOn)

main :: IO ()
main = do
  content <- readFile "problem011.txt"
  let l = [ read x :: Int | x <- foldl (++) [] $ map (splitOn " ") (lines content)]
  putStrLn $ show $ length l

-- check largestProductInSeries
