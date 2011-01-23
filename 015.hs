selfCompose :: (a -> a) -> Int -> a -> a
selfCompose f n = foldl (.) id fs
	where fs = replicate n f
	
nthRow :: (Integral a) => Int -> [a]
nthRow n = selfCompose nextRow n $ [1]
	where nextRow xs = zipWith (+) (0:xs) (xs ++ [0])

euler15 :: (Integral a) => Int -> Int -> a
euler15 rows columns = nthRow (rows + columns) !! rows