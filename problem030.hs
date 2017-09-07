import Data.Char (intToDigit)

-- digitFifthPower :: Int
-- digitFifthPower = sum $ [ sp | 
digitFifthPower = [ (sp, xs) | 
    a <- l, 
    b <- l, 
    c <- l, 
    d <- l, 
    e <- l, 
    let xs = [a, b, c, d, e],
    let sp = sumPower xs,
    show sp == toString xs ]
  where l = [0..9]

sumPower :: [Int] -> Int
sumPower xs = let p = length xs
               in sum $ map (^p) xs

toString :: [Int] -> String
toString xs = [ intToDigit x | x <- xs ]
