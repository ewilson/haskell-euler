import Data.List
import Data.Char

bigSum :: [[Int]] -> [Int]
bigSum = carry . map sum . transpose
	where
		carry (n:m:ms) = mod n 10 : carry ((m + quot n 10) : ms)
		carry (0:[]) = []
		carry (n:[]) = carry [n,0]

input :: String -> [[Int]]
input = map toBigInt . lines
	where toBigInt = reverse . map digitToInt

toString :: [Int] -> [Char]
toString = map intToDigit . reverse

euler13 :: String -> String
euler13 = take 10 . toString . bigSum . input
	
main = do
	input <- readFile "data/13.txt"
	putStrLn $ euler13 input