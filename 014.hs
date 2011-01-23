loop :: Int -> [Int]
loop 1 = [1]
loop n = n : (loop $ next n)
	where next n = 
		if mod n 2 == 0
			then quot n 2
			else 3 * n + 1
-- answer must be > N/2
-- answer must not be congruent to 2, 4, 5 mod 6