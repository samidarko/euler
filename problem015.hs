
countingLatticePath :: Int -> Integer
countingLatticePath n = head $ fn (take (n+1) $ repeat 1)
  where
    fn r@(x:[]) = r -- result
    fn xs = let t = tail xs
                h = head t * 2
             in fn $ foldl (\acc v -> acc ++ [last acc + v]) [h] (tail t)

