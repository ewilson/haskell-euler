import EulerMath.Triangle
import EulerUtil.Io

euler67 :: String -> String
euler67 = show . findMax . stringToIntArray

euler67file :: FilePath -> IO()
euler67file fileName = applyFunctionToFile fileName euler67
