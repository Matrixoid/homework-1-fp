module HW1.T1 
    ( Day(..)
    , nextDay
    , afterDays
    , isWeekend
    , daysToParty
    ) where
import Numeric.Natural

data Day = Monday 
         | Tuesday 
         | Wednesday 
         | Thursday 
         | Friday 
         | Saturday 
         | Sunday 
         deriving Show

nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday


afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays n day = nextDay (afterDays (mod n 7 - 1) day)

isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Monday = False
isWeekend Tuesday = False
isWeekend Wednesday = False
isWeekend Thursday = False
isWeekend Friday = False
isWeekend Saturday = True

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty d = (+) 1 (daysToParty (nextDay d))