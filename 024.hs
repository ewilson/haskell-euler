import EulerMath.BigNum
import Data.List

facts = map fact [10,9..1]

digits = [0..9]

linComb :: (Integral a) => a -> [a] -> [a]
linComb _ []     = []
linComb n (x:xs) = quot n x : (linComb (mod n x) xs) 

--permute :: (Integral a) => [a] -> [a] -> [a]
--permute xs (y:ys) = inc xs y : (permute rem

inc :: [Int] -> Int -> [Int]
inc xs y = newHead : (filter (/=newHead) sorted)
	where 
		sorted = sort xs
		newHead = sorted !! y