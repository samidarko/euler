import Data.List (elemIndex, maximumBy)

type Denominator = Int
type CycleLength = Int

reciprocalCycles :: (Denominator, CycleLength)
reciprocalCycles = 
    maximumBy (\a b -> compare (snd a) (snd b)) $ map cycleLength [1..999]

cycleLength :: Int -> (Denominator, CycleLength)
cycleLength d = fn 1 []
  where 
    fn n xs = case (n `elemIndex` xs) of
        Just(i) -> (d, length xs - i)
        _ -> fn ((n * 10) `mod` d) (xs ++ [n])

-- cycleLength == (983,982)
-- (0.00 secs, 1,034,472 bytes)

