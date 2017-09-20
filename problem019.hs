import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

data Date = Date { weekDay :: WeekDay, day :: Int, month :: Month, year :: Int } deriving (Show, Eq)

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

start = Date { weekDay = Tue, day = 1, month = Jan, year = 1901 }
end = Date { weekDay = Sun, day = 31, month = Dec, year = 2000 }

succDate d@(Date w 31 Dec y) = d { weekDay = succ w, day = 1, month = Jan, year = succ y }
succDate d@(Date w dd Feb y)
  | (isLeap y && dd == 29) || ((not . isLeap) y && dd == 28) = d { weekDay = succ w, day = 1, month = Mar}
  | otherwise = d { weekDay = succ w, day = succ dd }
succDate d@(Date w dd mm y)
  | isEndOfMonth = d { weekDay = succ w, day = 1, month = succ mm }
  | otherwise = d { weekDay = succ w, day = succ dd }
  where isEndOfMonth = (mm `elem` months31 && dd == 31) || (mm `elem` months30 && dd == 30)
        months31 = [Jan, Mar, May, Jul, Aug, Oct, Dec]
        months30 = [Apr, Jun, Sep, Nov]

instance Enum Date where
  succ =  succDate
  pred d = end
  fromEnum d = 1 -- from date to unix ts
  toEnum i = start -- from unix ts to date

isLeap :: Int -> Bool
isLeap x
  | x `mod` 100 == 0 = x `mod` 400 == 0 
  | otherwise = x `mod` 4 == 0

countingSundays :: Int
countingSundays = foldl (\acc v -> case v of
    Date Sun 1 _ _ -> acc + 1
    _ -> acc
                        ) 0 l
  where
    nextEnd = succ end
    l = takeWhile (/=nextEnd ) $ makeInfList start

makeInfList x = x : makeInfList (succ x)

-- countingSundays == 171

