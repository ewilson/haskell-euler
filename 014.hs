import qualified Data.Map as Map  
import Data.Maybe

loop :: (Integral a) => a -> [a]
loop 1 = [1]
loop n = n : (loop $ next n)

hasPreviousSmall :: (Integral a) => a -> Bool
hasPreviousSmall n = mod n 6 == 4 && n /= 4
 
previousSmall :: (Integral a) => a -> a
previousSmall n = quot (n-1) 3

allPrevious :: (Integral a) => a -> [a]
allPrevious n = 
	if (hasPreviousSmall n)
		then [n * 2, previousSmall n]
		else [n * 2]

listPrevious :: (Integral a) => [a] -> [a]
listPrevious [] = []
listPrevious (x:xs) = allPrevious x ++ listPrevious xs

		
next :: (Integral a) => a -> a
next n = 
	if mod n 2 == 0
		then quot n 2
		else 3 * n + 1
-- answer must be > N/2
-- answer must not be congruent to 2, 4, 5 mod 6
-- answer must not be congruent to 1 mod 6 unless > 750000

testMap = 
	[(1,1)
	,(2,2)
	,(3,8)
	,(4,3)
	,(5,6)
	]
	
newLoop :: (Integral a) => a -> [a]
newLoop n = takeWhile (>=n) $ loop n