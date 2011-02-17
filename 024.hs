import EulerMath.BigNum
import Data.List

facts = map fact [9,8..1]

digits :: [Int]
digits = [0..9]

linComb :: Int -> [Int] -> [Int]
linComb _ []     = []
linComb n (x:xs) = quot n x : (linComb (mod n x) xs) 

permute :: [Int] -> [Int] -> [Int]
permute xs [] = xs
permute xs (y:ys) = head increment : (permute (tail increment) ys)
	where 
		increment = inc xs y

inc :: [Int] -> Int -> [Int]
inc xs y = newHead : (filter (/=newHead) sorted)
	where 
		sorted = sort xs
		newHead = sorted !! y
		
euler24 :: Int -> [Int]
euler24 n = permute digits (linComb (n-1) facts) 