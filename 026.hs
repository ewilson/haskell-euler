import Data.List
import Data.Maybe

timesMod :: Int -> Int -> Int -> Int
timesMod modulus n m = mod (n*m) modulus

remainders :: Int -> [Int]
remainders denom = iterate (nextRemainder $ denom) 1
	where nextRemainder denom remain = timesMod denom remain 10

oneCycle :: [Int] -> [Int] -> [Int]
oneCycle previous (x:xs) =
	if elem x previous
		then dropWhile (/=x) previous
		else oneCycle (previous ++ [x]) xs

euler26 :: Int -> Int
euler26 n = 1 + (fromJust $ elemIndex (maximum cycles) cycles)
	where 
		cycles = map digitCycle [1..n]	
		digitCycle n = length $ oneCycle [] (remainders n)
