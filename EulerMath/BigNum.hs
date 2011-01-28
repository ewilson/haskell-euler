module EulerMath.BigNum  
( BigNat
, bigSum
, stringToBigNat
, bigNatToString
) where 

import Data.List
import Data.Char

type BigNat = [Int]

bigSum :: [BigNat] -> BigNat
bigSum = carry . map sum . transpose

carry :: BigNat -> BigNat
carry (n:m:ms) = mod n 10 : carry ((m + quot n 10) : ms)
carry (0:[]) = []
carry (n:[]) = carry [n,0]

stringToBigNat :: String -> BigNat
stringToBigNat = reverse . map digitToInt

bigNatToString :: BigNat -> [Char]
bigNatToString = map intToDigit . reverse

shift :: Int -> BigNat-> BigNat
shift n = (++) $ replicate n 0 

timesDigit :: (Int, Int) -> BigNat -> BigNat
timesDigit (value, place) = carry . shift place . map (*value)

withPlace :: BigNat -> [(Int, Int)]
withPlace big = zip big [0..]

foo :: [(Int, Int)] -> BigNat ->[BigNat]
foo (x:xs) ys = timesDigit x ys : foo xs ys
foo [] ys     = []

bar :: BigNat -> BigNat -> [BigNat]
bar = foo . withPlace

times :: BigNat -> BigNat -> BigNat
times x y = bigSum $ (foo . withPlace) x y