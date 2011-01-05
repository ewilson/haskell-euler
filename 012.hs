divisors :: Integer -> [Integer]
divisors n = [ d | d <- [1..n], mod n d == 0 ]

numDivs :: Integer -> Int
numDivs = length . divisors

tri :: Integer -> Integer
tri n = n * (n+1) `quot` 2

hm :: Integer -> Int
hm n = if (even n)
			then numDivs (quot n 2) * numDivs (n+1)
			else numDivs n * numDivs (quot (n+1) 2)

contenders :: Integer -> [Integer]
contenders n = twosMap [1..n]

twosMap :: [Integer] -> [Integer]
twosMap [] = []
twosMap (x:xs) = twos x ++ twosMap xs
		  
twos :: Integer -> [Integer]
twos n = (2^n - 1) : [2^n]

triDivTriple :: Integer -> (Integer, Integer, Int)
triDivTriple n = (n, tri n, hm n)

tripleAbove :: Int -> (Integer, Integer, Int) -> Bool
tripleAbove n (_, _, m) = (m > n)

{-- filter (tripleAbove 500) $ map triDivTriple $ contenders 20 --}