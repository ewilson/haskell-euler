divisors :: Int -> [Int]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

numDivs :: Int -> Int
numDivs = length . divisors

tri :: Int -> Int
tri n = n * (n+1) `quot` 2

hm :: Int -> Int
hm n = if (even n)
			then numDivs (quot n 2) * numDivs (n+1)
			else numDivs n * numDivs (quot (n+1) 2)

contenders :: Int -> [Int]
contenders n = twosMap [1..n]

twosMap :: [Int] -> [Int]
twosMap [] = []
twosMap (x:xs) = twos x ++ twosMap xs
		  
twos :: Int -> [Int]
twos n = (2^n - 1) : [2^n]