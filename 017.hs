onesWords = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
tensWords = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
teenWords = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

digitsToWords :: (Int, Int, Int) -> (String, String, String)
digitsToWords (h, t, o) = (onesWords !! h, tensWords !! t, onesWords !! o)

doubleToWords :: (Int, Int) -> String
doubleToWords (t, o) 
	| t == 1    = teenWords !! o
	| otherwise = hyphenate (tensWords !! t, onesWords !! o)
	where
		hyphenate ("", oWord) = oWord 
		hyphenate (tWord, "") = tWord 
		hyphenate (tWord, oWord) = tWord ++ "-" ++ oWord 

tripleToWords :: (Int, (Int, Int)) -> String
tripleToWords (h, to) = hundredAnd (onesWords !! h, doubleToWords to)
	where
		hundredAnd ("", toWord) = toWord
		hundredAnd (hWord, "") = hWord ++ " hundred"
		hundredAnd (hWord, toWord) = hWord ++ " hundred and " ++ toWord
		
makeTriple :: Int -> (Int, (Int, Int))
makeTriple n = (quot n 100, (mod (quot n 10) 10, mod n 10))

threeDigitsToWords :: Int -> String
threeDigitsToWords = tripleToWords . makeTriple
