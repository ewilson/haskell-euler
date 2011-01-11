module EulerMath.Divisors  
( divisors  
, primeFactorization
) where 

import EulerMath.Primes

{-- 
This is good for primes under 1000 currently.
A more general primeFactorization, both fast and
sufficient for larger numbers would be nice.
--}

divisors :: Integer -> [Integer]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

primes1000 :: [Int]
primes1000 = primesUpTo 1000

-- replacing primes1000 with primesUpTo n kills performance.
primeFactors :: Int -> [Int]
primeFactors n = [ d | d <- primes1000, mod n d == 0 ]

timesDivides :: Int -> Int -> Int
timesDivides d n = timesDividesIt d n 0
	where 
		timesDividesIt d n i = 
			if (mod n d /= 0)
				then i
				else timesDividesIt d (quot n d) (i+1)

primeFactorization :: Int -> [(Int, Int)]
primeFactorization n = map (\p -> (p, timesDivides p n)) $ primeFactors n