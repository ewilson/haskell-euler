euler1 :: Int -> Int
euler1 = sum . mult35
	where mult35 n = [ x | x <- [1..n-1], mod x 3 == 0 || mod x 5 == 0]