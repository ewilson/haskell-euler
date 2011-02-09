import EulerUtil.Io

input :: (Integral a, Read a) => String -> [a]
input = map read . lines

euler13 :: String -> String
euler13 = take 10 . show . sum . input

euler13file :: FilePath -> IO()
euler13file fileName = applyFunctionToFile fileName euler13
