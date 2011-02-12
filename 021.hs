properDivPairs :: (Integral a) => a -> [(a,a)]	
properDivPairs n = takeWhile (\(a, b) -> a <= b) [ (d, quot n d) | d <- [1..quot n 2], mod n d == 0 ]

d :: (Integral a) => a -> a
d n = divisorSum n - n
	where divisorSum = sum . map (uncurry (+)) . properDivPairs

hasAmicablePair :: (Integral a) => a -> Bool
hasAmicablePair n = d dn == n && n /= dn
	where dn = d n

euler21 :: (Integral a) => a -> a
euler21 n = sum $ filter hasAmicablePair [2..n]