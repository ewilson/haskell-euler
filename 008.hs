import Data.Char

findMax :: [Int] -> Int
findMax = head . foldl next [0,0,0,0,0]
	where next (oldMax: old: olds) new = max oldMax (old * new * product olds) : olds ++ [new] 

euler8 :: [Char] -> Int
euler8 = findMax . stringToNums
	where stringToNums = map digitToInt