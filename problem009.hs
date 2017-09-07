

specialPythagoreanTriplet :: [(Integer, Integer, Integer)]
specialPythagoreanTriplet = [ (a, b, c) | a <- [1..1000], b <- [a+1..1000], let c = 1000 - a - b, (a*a + b*b) == c*c ]


-- specialPythagoreanTriplet == [(200,375,425)]
-- product [200,375,425] == 31875000

