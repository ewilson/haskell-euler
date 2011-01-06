module SimpleMath  
( fib
, divisors  
, numDivs
) where 

fib :: Int -> Int
fib n = fst $ fibPair n
	where 
		fibPair 1 = (1,1)
		fibPair n = fibNext $ fibPair (n-1)
		fibNext (x,y) = (y, x+y)

divisors :: Integer -> [Integer]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

numDivs :: Integer -> Int
numDivs = length . divisors
