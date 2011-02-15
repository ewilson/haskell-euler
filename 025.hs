import EulerMath.Fib
import EulerMath.BigNum

euler25 :: (Integral a) => a -> a
euler25 n = head [ m | m <- [1..], numDigits (fib m) >= n ]