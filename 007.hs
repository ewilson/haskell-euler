simpleIsPrime :: Int -> Bool
simpleIsPrime n = null $ smallFactors n
	where smallFactors n = takeWhile (\x -> x^2 <= n) [d | d <- [2..n-1], mod n d == 0 ]
	
findPrime :: Int -> Int
findPrime 1 = 2
findPrime n = filter simpleIsPrime [3,5..] !! (n - 2)

relPrime :: Int -> Int -> Bool
relPrime d n = mod n d /= 0

sieve :: [Int] -> [Int] -> [Int]
sieve [] ys = ys
sieve (x:xs) ys = x : sieve xs (filter (relPrime x) ys)

sieveRange :: [Int] -> Int -> Int -> [Int]
sieveRange xs n m = sieve xs [n..m]

primes4 :: [Int]
primes4 = iterSieve [2,3] 4

iterSieve :: [Int] -> Int -> [Int]
iterSieve xs 0 = xs
iterSieve xs n = iterSieve (sieveRange xs (last xs) ((last xs)^2)) (n-1)

euler7 :: Int -> Int
euler7 n = primes4 !! (n - 1)