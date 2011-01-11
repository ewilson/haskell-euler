import EulerMath.Primes

euler10 :: Int -> Integer
euler10 = sum . map toInteger . primesUpTo
