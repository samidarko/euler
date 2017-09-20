import Data.List

-- highlyDivisibleTriangularNumber :: Int
hdtn :: Integral t => t -> t -> (t, t)
hdtn n total = let l = length $ factors total
                   s = succ n
               in if (l > 500) then (n, total) else  hdtn s (total + s)

factors :: Integral a => a -> [a]
factors n = ds ++ [r | (d,0) <- [divMod n r], r <- r:[d | d>r]] ++ reverse (map (n `div`) ds)
        where
        r = floor (sqrt (fromIntegral n))
        ds = [i | i <- [1..r-1], mod n i == 0]
