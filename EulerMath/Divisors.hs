module EulerMath.Divisors  
( d
, divisors  
, primeFactorization
, primeFactorizationFS
, primeFactors
) where 

import EulerMath.Primes

properDivPairs :: (Integral a) => a -> [(a,a)]	
properDivPairs n = takeWhile (\(a, b) -> a <= b) [ (d, quot n d) | d <- [1..quot n 2], mod n d == 0 ]

d :: (Integral a) => a -> a
d n = divisorSum n - n
	where 
		divisorSum = sum . map plus' . properDivPairs
		plus' (a, b) = 
			if a < b
				then a + b
				else a
				
divisors :: (Integral a) => a -> [a]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

primeFactors :: (Integral a) =>  a -> [a]
primeFactors n = [ d | d <- primesUpTo n, mod n d == 0 ]

primeFactorsFS :: (Integral a) => [a] -> a -> [a]
primeFactorsFS smallPrimes n = [ d | d <- smallPrimes, mod n d == 0 ]


timesDivides :: (Integral a) => a -> a -> a
timesDivides d n = timesDividesIt d n 0
	where 
		timesDividesIt d n i = 
			if (mod n d /= 0)
				then i
				else timesDividesIt d (quot n d) (i+1)

primeFactorization :: (Integral a) => a -> [(a, a)]
primeFactorization n = map (\p -> (p, timesDivides p n)) $ primeFactors n

primeFactorizationFS :: (Integral a) => [a] -> a -> [(a, a)]
primeFactorizationFS smallPrimes n = map (\p -> (p, timesDivides p n)) $ primeFactorsFS smallPrimes n