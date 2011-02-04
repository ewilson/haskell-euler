import Data.Char

onesWords = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tensWords = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
teenWords = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tensConnect = ("", "-")
hundredsConnect = (" hundred", " and ")
thousandsConnect = (" thousand", ", ")

-- Works up to 999,999.  Wouldn't take much to extend it much beyond that.
sayIt :: Int -> String
sayIt n = connect first second thousandsConnect
	where
		first = list !! 0
		second = list !! 1
		list = thousandsList n

thousandsList :: Int -> [String]
thousandsList = map threeDigitsToWords . commas 
	where 
		commas n = quot n 1000 : [mod n 1000]
		threeDigitsToWords = tripleToWords . makeTriple
		makeTriple n = (quot n 100, (mod (quot n 10) 10, mod n 10))

doubleToWords :: (Int, Int) -> String
doubleToWords (t, o) 
	| t == 1    = teenWords !! o
	| otherwise = connect (tensWords !! t) (onesWords !! o) tensConnect

tripleToWords :: (Int, (Int, Int)) -> String
tripleToWords (h, to) = connect (onesWords !! h) (doubleToWords to) hundredsConnect

connect :: String -> String -> (String, String) -> String
connect "" second connect = second
connect first "" connect = first ++ fst connect
connect first second connect = first ++ fst connect ++ snd connect ++ second
		
totalLetters :: [String] -> Int
totalLetters words = sum $ map lettersInString words
	where lettersInString = length . filter isLetter 

euler17 :: Int -> Int
euler17 = totalLetters . countTo
	where countTo n = map sayIt [1..n]