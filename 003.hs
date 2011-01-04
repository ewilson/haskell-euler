{-- 
	There must be a better way.  
	Sorry this is unreadable, and resorts to using Integer.
--}

isPrime :: Integer -> Bool
isPrime =  null . smallFactors 

smallFactors :: Integer -> [Integer]
smallFactors n = takeWhile (\d -> d*d <= n) $ factors n
	where factors n = [ d | d <- [2..n-1], mod n d == 0]

smallPrimeFactors :: Integer -> [Integer]
smallPrimeFactors = filter isPrime . smallFactors
	
reduceByList :: Integer -> [Integer] -> Integer
reduceByList big []         = big
reduceByList big (div:divs) = reduceByList (reduce big div) divs
	where reduce num div =
		if mod num div /= 0
			then num
			else reduce (quot num div) div

euler3 :: Integer -> Integer
euler3 n = if reduceByList n (smallPrimeFactors n) == 1 
				then last (smallPrimeFactors n)
				else reduceByList n (smallPrimeFactors n)