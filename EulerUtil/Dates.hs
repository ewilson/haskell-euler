module EulerUtil.Dates  
( Year
, Month
, Day
, Date
, isLeapYear
, daysInYear
, daysInMonth  
) where 

type Year = Int
type Month = Int
type Day = Int

data Date = Date { year :: Year  
                 , month :: Month  
				 , day :: Day
				 } deriving (Show)

isLeapYear :: Year -> Bool
isLeapYear year
	| mod year 400 == 0 = True
	| mod year 100 == 0 = False
	| mod year 4 == 0   = True
	| otherwise         = False

daysInYear :: Year -> Int
daysInYear year = 
	if isLeapYear year
		then 366
		else 365

daysInMonth :: Date -> Int
daysInMonth date
	| elem m [1,3,5,7,8,10,12] = 31
	| m == 2                   = if isLeapYear y then 29 else 28
	| otherwise	 			   = 30
		where 
			m = month date
			y = year date
			