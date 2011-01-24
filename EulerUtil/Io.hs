module EulerUtil.Io  
( applyFunctionToFile   
) where 

applyFunctionToFile :: FilePath -> (String -> String) -> IO()
applyFunctionToFile fileName f = do
	input <- readFile fileName
	putStrLn $ f input

