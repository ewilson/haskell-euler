import Data.List
import Data.Char
import EulerUtil.Io

type BigNat = [Int]

bigSum :: [BigNat] -> BigNat
bigSum = carry . map sum . transpose
	where
		carry (n:m:ms) = mod n 10 : carry ((m + quot n 10) : ms)
		carry (0:[]) = []
		carry (n:[]) = carry [n,0]

input :: String -> [BigNat]
input = map toBigNat . lines
	where toBigNat = reverse . map digitToInt

toString :: BigNat -> [Char]
toString = map intToDigit . reverse

euler13 :: String -> String
euler13 = take 10 . toString . bigSum . input

euler13file :: FilePath -> IO()
euler13file fileName = applyFunctionToFile fileName euler13
