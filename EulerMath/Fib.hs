module EulerMath.Fib  
( fib
) where 

fib :: Int -> Int
fib n = fst $ fibPair n
	where 
		fibPair 1 = (1,1)
		fibPair n = fibNext $ fibPair (n-1)
		fibNext (x,y) = (y, x+y)
