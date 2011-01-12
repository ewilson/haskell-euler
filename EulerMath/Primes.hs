module EulerMath.Primes  
( nPrimes
, primesUpTo  
, primesUpToRoot
, isPrime
) where 

divides :: (Integral a) => a -> a -> Bool
divides d n = mod n d == 0

sieve :: (Integral a) => [a] -> [a] -> [a]
sieve [] ys = ys
sieve (x:xs) ys = x : sieve xs (filter (not . divides x) ys)

genPrimes :: (Integral a) => a -> [a]
genPrimes n = iterSieve [2,3] n
	where
		iterSieve xs 0 = xs
		iterSieve xs n = iterSieve (sieveRange xs (last xs) ((last xs)^2)) (n-1)
		sieveRange xs n m = sieve xs [n..m]

nPrimes :: (Integral a) => Int -> [a]
nPrimes n = take n $ genPrimes 4

primesUpTo :: (Integral a) =>  a -> [a]
primesUpTo n = takeWhile (<=n) $ genPrimes 4

primesUpToRoot :: (Integral a) => a -> [a]
primesUpToRoot n = takeWhile (\d -> d^2 <= n) $ genPrimes 4

isPrime :: (Integral a) => a -> Bool
isPrime n = any (==n) $ primesUpTo (n+1)