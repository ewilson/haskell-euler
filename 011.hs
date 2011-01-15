stringToIntArray :: String -> [[Int]]
stringToIntArray = stringArrayToIntArray . stringToArray 
	where 
		stringToArray = map words . lines
		stringArrayToIntArray = map $ map read
		
findMax :: [Int] -> Int
findMax = head . foldl next [0,0,0,0]
	where next (oldMax: old: olds) new = max oldMax (old * new * product olds) : olds ++ [new] 

diagSum :: [[a]] -> Int -> [a]
diagSum _ (-1)          = []
diagSum (row:rows) idx 
	| idx < 0           = []
	| idx >= length row = diagSum rows (idx - 1)
	| otherwise         = row !! idx : diagSum rows (idx - 1) 
diagSum [] _            = []

z = [[1,2,3],[4,5,6]]

constSumDiags :: [[a]] -> [[a]]
constSumDiags array = map (diagSum array) [0..n]
	where n = length $ head array