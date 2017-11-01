
largeNonMersennePrime :: Integer
largeNonMersennePrime = let a = 28433 * 2^7830457 + 1
                            b = 10000000000
                         in a `mod` b

-- *Main> largeNonMersennePrime
-- 8739992577
-- (0.08 secs, 5,885,008 bytes)

