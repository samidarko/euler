
sumSquareDifference :: Int
sumSquareDifference = 
    let boundary = 100
        sumSquare = sum [i^2 | i <- [1..100]]
        squareSum = sum [1..100] ^ 2 
    in squareSum - sumSquare
