import Helpers (takeEvery)

numberSpiralDiagonals :: Int
numberSpiralDiagonals = sum $ fn [1,3..1001] 1
  where 
    fn [] _ = []
    fn (x:xs) start = let end = x * x
                       in (sum $ takeEvery (x-1) [start .. end]) : fn xs (end+1)

-- numberSpiralDiagonals == 669171001

