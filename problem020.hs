import Helpers (stringToIntList)

factorialDigitSum :: Integer -> Integer
factorialDigitSum n = aggregate $ product [1 .. n]
  where aggregate = (sum . stringToIntList . show)


