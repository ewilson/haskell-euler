import EulerMath.BigNum

bigExp' :: Int -> Int -> BigNat
bigExp' base = bigExp (intToBigNat base)

euler16 :: Int -> Int -> Int
euler16 base = sum . bigExp' base