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
         deriving (Show, Eq, Ord)

nextDay :: Day -> Day
nextDay day = case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays count day
  | mod count 7 == 0 = day
  | count > 7        = nextDay (afterDays (mod count 7 - 1) day)
  | count > 0        = nextDay (afterDays (count - 1) day)

isWeekend :: Day -> Bool
isWeekend day = (day == Saturday) || (day == Sunday)

daysToParty :: Day -> Natural
daysToParty day
  | day == Friday = 0
  | otherwise     = daysToParty (nextDay day) + 1