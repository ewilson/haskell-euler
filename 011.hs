stringToIntArray :: String -> [[Int]]
stringToIntArray = stringArrayToIntArray . stringToArray 
	where 
		stringToArray = map words . lines
		stringArrayToIntArray = map $ map read
		
findMax :: [Int] -> Int
findMax = head . foldl next [0,0,0,0]
	where next (oldMax: old: olds) new = max oldMax (old * new * product olds) : olds ++ [new] 
