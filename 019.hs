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
			
epoch :: Date
epoch = Date 1900 1 1

epochDayOfWeek :: Int
epochDayOfWeek = 1
			
daysInPreviousYearsSinceEpoch :: Year -> Int
daysInPreviousYearsSinceEpoch currentYear = sum $ map daysInYear [year epoch..currentYear-1]

daysInPreviousMonthsInCurrentYear :: Date -> Int
daysInPreviousMonthsInCurrentYear date = sum $ map daysInMonth (previousMonths date)

previousMonths :: Date -> [Date]
previousMonths date = [ Date (year date) m 1 | m <- [1..(month date)-1]]

daysSinceEpoch :: Date -> Int
daysSinceEpoch date = daysInPreviousYearsSinceEpoch (year date)
					  + daysInPreviousMonthsInCurrentYear date
					  + (day date - 1)

monthsInYears :: Year -> Year -> [Date]
monthsInYears start end = [ Date y m 1 | y <- [start..end], m<-[1..12] ]
					  
mod7 :: Int -> Int
mod7 n = mod n 7

dayOfWeek :: Date -> Int
dayOfWeek date = mod7 $ epochDayOfWeek + daysSinceEpoch date

isSunday :: Date -> Bool
isSunday date = dayOfWeek date == 0