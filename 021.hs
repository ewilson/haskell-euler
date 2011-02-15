import EulerMath.Divisors

hasAmicablePair :: (Integral a) => a -> Bool
hasAmicablePair n = d dn == n && n /= dn
	where dn = d n

euler21 :: (Integral a) => a -> a
euler21 n = sum $ filter hasAmicablePair [2..n]