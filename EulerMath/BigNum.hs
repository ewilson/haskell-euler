module EulerMath.BigNum  
( fact
, digitSum ) where 

digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = mod n 10 + digitSum (quot n 10)

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)