module EulerMath.Primes  
( nPrimes
, primesUpTo  
, primesBelowRoot
, isPrime
) where 

divides :: Int -> Int -> Bool
divides d n = mod n d == 0

sieve :: [Int] -> [Int] -> [Int]
sieve [] ys = ys
sieve (x:xs) ys = x : sieve xs (filter (not . divides x) ys)

genPrimes :: Int -> [Int]
genPrimes n = iterSieve [2,3] n
	where
		iterSieve xs 0 = xs
		iterSieve xs n = iterSieve (sieveRange xs (last xs) ((last xs)^2)) (n-1)
		sieveRange xs n m = sieve xs [n..m]

nPrimes :: Int -> [Int]
nPrimes n = take n $ genPrimes 4

primesUpTo :: Int -> [Int]
primesUpTo n = takeWhile (<=n) $ genPrimes 4

primesBelowRoot :: Int -> [Int]
primesBelowRoot n = takeWhile (\d -> d^2 <= n) $ genPrimes 4

isPrime :: Int -> Bool
isPrime n = any (==n) $ primesUpTo (n+1)