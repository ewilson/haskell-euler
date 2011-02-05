import EulerMath.Triangle
import EulerUtil.Io

euler18 :: String -> String
euler18 = show . findMax . stringToIntArray

euler18file :: FilePath -> IO()
euler18file fileName = applyFunctionToFile fileName euler18
