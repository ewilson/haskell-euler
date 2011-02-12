import EulerMath.Divisors

-- I could optimize this, or I could move on to other problems . . .
properDivs :: (Integral a) => a -> [a]
properDivs n = [ d | d <- [1..n2], mod n d == 0 ]
	where n2 = quot n 2

d :: (Integral a) => a -> a
d = sum . properDivs

hasAmicablePair :: (Integral a) => a -> Bool
hasAmicablePair n = d dn == n && n /= dn
	where dn = d n

euler21 :: (Integral a) => a -> a
euler21 n = sum $ filter hasAmicablePair [2..n]