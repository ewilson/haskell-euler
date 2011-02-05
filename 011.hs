import Data.List
import EulerUtil.Io

findMaxInRow :: (Integral a) => [a] -> a
findMaxInRow = head . foldl next [0,0,0,0]
	where next (oldMax: old: olds) new = max oldMax (old * new * product olds) : olds ++ [new] 

findRowMaxInArray :: (Integral a) => [[a]] -> a
findRowMaxInArray = maximum . map findMaxInRow

findMax :: (Integral a) => [[a]] -> a
findMax = maximum . map findRowMaxInArray . findArrays
	where findArrays array = map ($ array) [id, transpose, constSumDiags, constDiffDiags]

constSumDiags :: [[a]] -> [[a]]
constSumDiags array = map (diagSum array) [0..n]
	where 
		diagSum (row:rows) idx 
			| idx < 0           = []
			| idx >= length row = diagSum rows (idx - 1)
			| otherwise         = row !! idx : diagSum rows (idx - 1) 
		diagSum [] _            = []
		n = (length $ head array) + (length array) - 2

constDiffDiags :: [[a]] -> [[a]]
constDiffDiags array = map (diagDiff array) [m..n]
	where 
		diagDiff (row:rows) idx 
			| idx < 0           = diagDiff rows (idx + 1)
			| idx >= length row = []
			| otherwise         = row !! idx : diagDiff rows (idx + 1) 
		diagDiff [] _           = []
		m = ((-1) * length array) + 1
		n = (length $ head array) - 1
		
euler11 :: String -> String
euler11 = show . findMax . stringToIntArray

euler11file :: FilePath -> IO()
euler11file fileName = applyFunctionToFile fileName euler11
