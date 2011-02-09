import EulerMath.BigNum

euler16 :: (Integral a) => a -> a -> a
euler16 base = digitSum . (base^)
