stringToIntArray :: String -> [[Int]]
stringToIntArray = stringArrayToIntArray . stringToArray 
	where 
		stringToArray = map words . lines
		stringArrayToIntArray = map $ map read