import EulerUtil.Io
import Data.List
import Data.Char

inputToSortedList :: String -> [String]
inputToSortedList = sort . splitOn ','

score :: String -> Int
score = sum . map letToInt

letToInt :: Char -> Int
letToInt c = 
	if isLetter c
		then ord c - ord 'A' + 1
		else 0

weightScores :: [Int] -> Int
weightScores = sum . zipWith (*) [1..]

euler22 :: String -> String
euler22 = show . weightScores . map score . inputToSortedList

euler22file :: FilePath -> IO()
euler22file fileName = applyFunctionToFile fileName euler22

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c (s:ss) 
	| c == s    = splitOn c ss 
	| otherwise = takeWhile (/=c) (s:ss) : (splitOn c $ dropWhile (/=c) (s:ss))