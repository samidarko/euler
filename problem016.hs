module Problem20 (
    powerDigitSum
                 ) where

import Helpers (stringToIntList)

powerDigitSum :: Integer -> Integer
powerDigitSum x = (sum . stringToIntList . show) (2^x)


