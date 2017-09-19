import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (findIndex)

data Date = Date { weekDay :: WeekDay, day :: Int, month :: Month, year :: Int } deriving (Show)

data WeekDay = 
    Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq, Ord)

weekdayList = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]
weekdayMap = M.fromList (weekdayList  `zip` [1..])

data Month = 
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Eq, Ord)

monthList = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]
monthMap = M.fromList (monthList  `zip` [1..])

instance Enum WeekDay where
  succ d = case (findIndex (d==) weekdayList) of
      Just(x) -> if (x == 6) then Mon else weekdayList !! (x + 1)
  pred d = case (findIndex (d==) weekdayList) of
      Just(x) -> if (x == 0) then Sun else weekdayList !! (x - 1)
  fromEnum d = fromJust $ M.lookup d weekdayMap
  toEnum i = weekdayList !! i

instance Enum Month where
  succ d = case (findIndex (d==) monthList) of
      Just(x) -> if (x == 11) then Jan else monthList !! (x + 1)
  pred d = case (findIndex (d==) monthList) of
      Just(x) -> if (x == 0) then Dec else monthList !! (x - 1)
  fromEnum d = fromJust $ M.lookup d monthMap
  toEnum i = monthList !! i

countingSundays :: Int
countingSundays = 1

-- calendar = 
-- fn x = x : fn (succ x)
