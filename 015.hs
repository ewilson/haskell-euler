pascal :: (Integral a) => Int -> [[a]]
pascal n = take (n+1) $ iterate nextRow [1]
	where nextRow xs = zipWith (+) (0:xs) (xs ++ [0])

binom :: (Integral a) => Int -> Int -> a
binom n k = last (pascal n) !! k
	
euler15 :: (Integral a) => Int -> Int -> a
euler15 rows columns = binom (rows + columns) columns
