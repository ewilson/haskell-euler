findMultiple :: Int -> Int -> Int
findMultiple currentProd next =
	if mod currentProd next == 0 
		then currentProd
		else currentProd * firstPrimeDivisor next
			where firstPrimeDivisor n = head [ d | d <- [2..n], mod n d == 0 ]
	
euler5 :: Int -> Int
euler5 n = foldl findMultiple 1 [1..n]