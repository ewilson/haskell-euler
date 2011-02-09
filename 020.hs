import EulerMath.BigNum

euler20 :: (Integral a) => (a -> a)
euler20 = digitSum . fact