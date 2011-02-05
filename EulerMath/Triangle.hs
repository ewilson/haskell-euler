module EulerMath.Triangle  
( findMax
) where 

type Row = [Int]

compressTwo :: Row -> Row -> Row
compressTwo lower upper = zipWith (+) maxLower upper
	where 
		maxLower = zipWith max (init lower) (tail lower)

compressAll :: [Row] -> Row
compressAll (r:[]) = r
compressAll (r:r':rs) = compressAll (compressTwo r r':rs) 
	
findMax :: [Row] -> Int
findMax = head . compressAll . reverse
