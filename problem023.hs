import Helpers (factors)

abondantNumbers = [ x | x <- [1..28123], let y = sum $ factors x, x < y]


