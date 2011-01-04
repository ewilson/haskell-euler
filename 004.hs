numToList :: Int -> [Int]
numToList 0 = []
numToList x = mod x 10 : numToList (quot x 10)

palNum :: Int -> Bool
palNum = isPal . numToList
	where isPal x = (x == reverse x)

euler4 :: Int -> Int -> Int
euler4 x y = maximum $ palProds x y
	where palProds min max = filter palNum [x*y | x <- [min..max], y <- [min..max], y >= x ]
