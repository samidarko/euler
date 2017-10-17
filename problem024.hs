
-- https://www.youtube.com/watch?v=bSFIjJi2syQ&index=31&list=PLo_ebKiwsNTpJmp47_lQJYuKqk6XVjf2a

permutations n digits = 
    let digitsLength = length digits
     in if (digitsLength == 1) 
           then digits 
           else 
           let groupSize = fact $ digitsLength-1 
               groupIndex = n `div` groupSize
               withinGroup = n `mod` groupSize
               value = digits !! groupIndex
            in value : permutations withinGroup (filter (/= value) digits)

fact x = product [1..x]

-- (map intToDigit $ permutations 999999 [0..9]) == "2783915460"

