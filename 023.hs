import EulerMath.Divisors

isAbundant :: Int -> Bool
isAbundant n = d n > n

abundants :: [Int]
abundants = filter isAbundant [12..28123]

sumTwoAbundants :: Int -> Bool
sumTwoAbundants = sumOfList abundants

sumOfList :: [Int] -> Int -> Bool
sumOfList [] n = False
sumOfList (x:xs) n 
	| n < 2*x                                = False
	| elem (n-x) $ takeWhile (<= n-x) (x:xs) = True
	| otherwise                              = sumOfList xs n
	
euler23 :: Int -> Int
euler23 n = sum $ filter (not . sumTwoAbundants) [2,4..n]