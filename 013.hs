import Data.List

bigSum :: [[Int]] -> [Int]
bigSum = carry . map sum . transpose

carry :: [Int] -> [Int]
carry (n:m:ms) = mod n 10 : carry ((m + quot n 10) : ms)
carry (0:[]) = []
carry (n:[]) = carry [n,0]