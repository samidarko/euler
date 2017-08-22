

smallestMultiple :: Int -> Int
smallestMultiple x = case (helper x 1) of
                        Just y -> y
                        _ -> smallestMultiple (x+1)


helper :: Int -> Int -> Maybe Int
helper x i
  | isModZero && i == limit = Just x
  | isModZero && i < limit = helper x (i+1)
  | otherwise = Nothing
  where isModZero = x `mod` i == 0
        limit = 20

-- smallestMultiple 2521 == 232792560
