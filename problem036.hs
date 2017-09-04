import Data.Char (intToDigit)
import Numeric (showIntAtBase)

toBinary :: Int -> String
toBinary x = showIntAtBase 2 intToDigit x ""

doubleBasePalindromes :: Int -> Int
doubleBasePalindromes limit = sum [ x | 
    x <- [0..limit], 
    let y = show x, 
    let b = toBinary x, 
        y == reverse y, 
        b == reverse b]

-- doubleBasePalindromes 1000000 == 872187

