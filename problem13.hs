
largeSum :: IO ()
largeSum = do
  content <- readFile "problem13.txt"
  let l = [ read x :: Integer | x <- lines content]
  putStrLn $ take 10 $ show $ sum l

