module SimpleMath  
( divisors  
, numDivs
) where 

divisors :: Integer -> [Integer]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

numDivs :: Integer -> Int
numDivs = length . divisors
