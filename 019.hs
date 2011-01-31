import EulerUtil.Dates
			
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

startsWithSunday :: Year -> Year -> [Date]
startsWithSunday start end = filter isSunday $ monthsInYears start end