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
	where
		carry (n:m:ms) = mod n 10 : carry ((m + quot n 10) : ms)
		carry (0:[]) = []
		carry (n:[]) = carry [n,0]

stringToBigNat :: String -> BigNat
stringToBigNat = reverse . map digitToInt

bigNatToString :: BigNat -> [Char]
bigNatToString = map intToDigit . reverse

