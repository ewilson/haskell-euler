module EulerMath.BigNum  
( BigNat
, bigSum
, times
, bigExp
, bigFact
, stringToBigNat
, bigNatToString
, intToBigNat
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

intToBigNat :: Int -> BigNat
intToBigNat n = mod n 10 : 
	if next > 0
		then intToBigNat next
		else []
	where next = quot n 10

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

two :: [Int]
two = [2]
		
bigExp :: (Integral a) => BigNat -> a -> BigNat
bigExp base 1 = base
bigExp base n = 
	if even n
		then sqr $ bigExp base (quot n 2)
		else times base $ bigExp base (n - 1)
	
sqr :: BigNat -> BigNat
sqr big = times big big

bigFact :: Int -> BigNat
bigFact 0 = [1]
bigFact n = times (intToBigNat n) (bigFact (n-1))