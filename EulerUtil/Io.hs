module EulerUtil.Io  
( applyFunctionToFile   
, stringToIntArray 
) where 

applyFunctionToFile :: FilePath -> (String -> String) -> IO()
applyFunctionToFile fileName f = do
	input <- readFile fileName
	putStrLn $ f input

stringToIntArray :: String -> [[Int]]
stringToIntArray = stringArrayToIntArray . stringToArray 
	where 
		stringToArray = map words . lines
		stringArrayToIntArray = map $ map read
