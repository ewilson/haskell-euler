import EulerMath.Primes
import EulerMath.Divisors

firstPrimeFactor :: Integer -> Integer
firstPrimeFactor n = head [ d | d <- primesUpTo n, mod n d == 0]

reduceByNextPrime :: Integer -> Integer
reduceByNextPrime n = reduce n (firstPrimeFactor n)
	where reduce num div =
		if mod num div /= 0
			then num
			else reduce (quot num div) div
	
euler3 :: Integer -> Integer
euler3 n = 
	if reduced == 1
		then n
		else euler3' reduced
			where reduced = reduceByNextPrime n