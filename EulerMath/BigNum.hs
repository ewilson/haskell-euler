module EulerMath.BigNum  
( fact
, digitSum
, numDigits
 ) where 

digitSum :: (Integral a) => a -> a
digitSum 0 = 0
digitSum n = mod n 10 + digitSum (quot n 10)

numDigits :: (Integral a) => a -> a
numDigits 0 = 0
numDigits n = 1 + numDigits (quot n 10)

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)