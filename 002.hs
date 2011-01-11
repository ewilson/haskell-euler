import EulerMath.Fib

euler2 :: Int -> Int
euler2 n = sum $ filter even $ takeWhile (< n) [ fib k | k <- [1..] ]