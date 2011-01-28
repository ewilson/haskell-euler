module EulerMath.BigNum  
( BigNat
, bigSum
, times
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

times :: BigNat -> BigNat -> BigNat
times x y = bigSum $ (timesPlaces . withPlace) x y
	where
		timesPlaces (x:xs) ys = timesDigit x ys : timesPlaces xs ys
		timesPlaces [] ys     = []
		timesDigit (value, place) = carry . shift place . map (*value)
		shift n = (++) $ replicate n 0 
		withPlace big = zip big [0..]
