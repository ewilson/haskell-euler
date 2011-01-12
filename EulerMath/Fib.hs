module EulerMath.Fib  
( fib
) where 

fib :: (Integral a) => a -> a
fib n = fst $ fibPair n
	where 
		fibPair 1 = (1,1)
		fibPair n = fibNext $ fibPair (n-1)
		fibNext (x,y) = (y, x+y)
