corners :: Int -> Int
corners n = (4 * oddSquare) - (6 * even)
	where 
		oddSquare = n^2
		even = n-1

euler28 :: Int -> Int
euler28 size = 1 + (sum $ map corners [3,5..size])