module EulerMath.Divisors  
( divisors  
, numDivs
, primeFactorization
) where 

import EulerMath.Primes

divisors :: Integer -> [Integer]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

numDivs :: Integer -> Int
numDivs = length . divisors

primeFactors :: Int -> [Int]
primeFactors n = [ d | d <- primesUpTo n, mod n d == 0 ]

timesDivides :: Int -> Int -> Int
timesDivides d n = timesDividesIt d n 0
	where 
		timesDividesIt d n i = 
			if (mod n d /= 0)
				then i
				else timesDividesIt d (quot n d) (i+1)

primeFactorization :: Int -> [(Int, Int)]
primeFactorization n = map (\p -> (p, timesDivides p n)) $ primeFactors n