import EulerMath.Divisors
import EulerMath.Primes

tri :: Int -> Int
tri n = n * (n+1) `quot` 2

triDivs :: Int -> Int
triDivs n = if (even n)
				then numDivs (quot n 2) * numDivs (n+1)
				else numDivs n * numDivs (quot (n+1) 2)

numDivs :: Int -> Int
numDivs n = product $ map (+1) $ primeFactorMultiplicity n

primeFactorMultiplicity :: Int -> [Int]
primeFactorMultiplicity = map snd . primeFactorizationFS primes1000
		where primes1000 = primesUpTo 1000

euler12 :: Int -> Int
euler12 n = (\ (_, t, _) -> t) . head $ filter (tripleAbove n) $ map triDivTriple [1..]
	where 
		triDivTriple m = (m, tri m, triDivs m)
		tripleAbove n (_, _, m) = (m > n)



