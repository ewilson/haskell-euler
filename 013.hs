import EulerUtil.Io
import EulerMath.BigNum

input :: String -> [BigNat]
input = map stringToBigNat . lines

euler13 :: String -> String
euler13 = take 10 . bigNatToString . bigSum . input

euler13file :: FilePath -> IO()
euler13file fileName = applyFunctionToFile fileName euler13
