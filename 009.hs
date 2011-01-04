triplesOfSum :: Int -> [[Int]]
triplesOfSum n = [[n-z-y,y,z] | z <- [1..quot n 2], y <- [z, z-1..1], (n - z - y)^2 + y^2 == z^2 ]

euler9 :: Int -> Int
euler9 = product . head . triplesOfSum