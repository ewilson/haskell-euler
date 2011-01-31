import EulerUtil.Dates
			
epoch :: Date
epoch = Date 1900 1 1

epochDayOfWeek :: Int
epochDayOfWeek = 1
			
daysSinceEpoch :: Date -> Int
daysSinceEpoch date = daysInPreviousYearsSinceEpoch (year date)
					  + daysInPreviousMonthsInCurrentYear date
					  + (day date - 1)
	where 
		daysInPreviousMonthsInCurrentYear date = sum $ map daysInMonth (previousMonths date)
		daysInPreviousYearsSinceEpoch currentYear = sum $ map daysInYear [year epoch..currentYear-1]
		previousMonths date = [ Date (year date) m 1 | m <- [1..(month date)-1]]

dayOfWeek :: Date -> Int
dayOfWeek date = (epochDayOfWeek + daysSinceEpoch date) `mod` 7

startsWithSunday :: (Year, Year) -> [Date]
startsWithSunday = filter isSunday . monthsInYears
	where
		monthsInYears (start, end) = [ Date y m 1 | y <- [start..end], m<-[1..12] ]
		isSunday = (==0) . dayOfWeek

euler19 :: (Year, Year) -> Int
euler19 = length . startsWithSunday