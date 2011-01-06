import SimpleMath

tri :: Integer -> Integer
tri n = n * (n+1) `quot` 2

triDivs :: Integer -> Int
triDivs n = if (even n)
			then numDivs (quot n 2) * numDivs (n+1)
			else numDivs n * numDivs (quot (n+1) 2)

euler12 :: Int -> (Integer, Integer, Int)
euler12 n = head $ filter (tripleAbove n) $ map triDivTriple [1..]
	where triDivTriple m = (m, tri m, triDivs m)
		  tripleAbove n (_, _, m) = (m > n)
