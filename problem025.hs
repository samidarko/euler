import Helpers (fibs)

thousandFibNumber :: Int
thousandFibNumber = fn (zip fibs [1..])
  where fn (x:xs) = if ((length $ show $ fst x) >= 1000) then snd x else fn xs

