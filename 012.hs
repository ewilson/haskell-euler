import EulerMath.Divisors
import EulerMath.Primes

tri :: Int -> Int
tri n = n * (n+1) `quot` 2

triDivs :: Int -> Int
triDivs n = if (even n)
				then numDivs (quot n 2) * numDivs (n+1)
				else numDivs n * numDivs (quot (n+1) 2)

euler12 :: Int -> (Int, Int, Int)
euler12 n = head $ filter (tripleAbove n) $ map triDivTriple [1..]
	where 
		triDivTriple m = (m, tri m, triDivs m)
		tripleAbove n (_, _, m) = (m > n)

primeFactorsWithMult :: Int -> [Int]
primeFactorsWithMult = map snd . primeFactorization

numDivs :: Int -> Int
numDivs n = product $ map (+1) $ primeFactorsWithMult n
