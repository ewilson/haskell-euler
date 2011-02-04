import Data.Char

onesWords = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tensWords = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
teenWords = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tensConnect = ("", "-")
hundredsConnect = (" hundred", " and ")
thousandsConnect = (" thousand", " , ")

digitsToWords :: (Int, Int, Int) -> (String, String, String)
digitsToWords (h, t, o) = (onesWords !! h, tensWords !! t, onesWords !! o)

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
		
makeTriple :: Int -> (Int, (Int, Int))
makeTriple n = (quot n 100, (mod (quot n 10) 10, mod n 10))

threeDigitsToWords :: Int -> String
threeDigitsToWords = tripleToWords . makeTriple

commas :: Int -> [Int]
commas 0 = []
commas n = mod n 1000 : (commas $ quot n 1000)

sayIt :: Int -> String
sayIt n = "num-ber ,"

countTo :: Int -> [String]
countTo n = map sayIt [1..n]

totalLetters :: [String] -> Int
totalLetters words = sum $ map lettersInString words
	where lettersInString = length . filter isLetter 

euler17 :: Int -> Int
euler17 = totalLetters . countTo