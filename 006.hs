squareOfSum :: Int -> Int
squareOfSum n = (sumFirstNums n) ^ 2
	where sumFirstNums n = quot (n * (n + 1)) 2

sumOfSquares :: Int -> Int
sumOfSquares n = quot (n * (n+1) * (2*n + 1)) 6

euler6 :: Int -> Int
euler6 n = squareOfSum n - sumOfSquares n