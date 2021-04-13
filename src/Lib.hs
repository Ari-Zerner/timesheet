module Lib where

import Result
import Data.Time
import Text.Printf
import Data.Maybe (isJust)

type Time = UTCTime

getCurrentTime :: IO Time
getCurrentTime = Data.Time.getCurrentTime

type Shift = (Time, Time)

type Period = [Shift]

type Duration = NominalDiffTime

newtype Hours = Hours Double

instance Show Hours where
  show (Hours h) = printf "%.2f hours" h

-- lists should be small enough in this application that cons performance doesn't matter, and it's easier to think about lists this way
(+:) :: [a] -> a -> [a]
xs +: x = xs ++ [x]

(~~) :: Time -> Time -> Duration
(~~) = flip diffUTCTime

shiftDuration :: Shift -> Duration
shiftDuration (start, end) = start ~~ end

periodDuration :: Period -> Duration
periodDuration = sum . map shiftDuration

durationToHours :: Duration -> Hours
durationToHours = Hours . fromRational . (/ 3600) . toRational

data Timesheet = Timesheet { start         :: Maybe Time
                           , currentPeriod :: Period
                           , pastPeriods   :: [Period]
                           }
  deriving Show -- for debugging only

blankTimesheet :: Timesheet
blankTimesheet = Timesheet { start         = Nothing
                           , currentPeriod = []
                           , pastPeriods   = []
                           }

clockIn :: Time -> Timesheet -> Result Timesheet
clockIn t ts = case start ts of
  Nothing -> return ts { start = Just t }
  Just _ -> fail "already clocked in"
    
clockOut :: Time -> Timesheet -> Result (Timesheet, Duration)
clockOut t ts = resultFromMaybe "not clocked in" $ (\st -> (update t st, st ~~ t)) <$> start ts
  where update end start = ts { start = Nothing
                              , currentPeriod = currentPeriod ts +: (start, end)
                              }

clockedIn :: Timesheet -> Bool
clockedIn = isJust . start

-- (start time, length so far)
timeThisShift :: Time -> Timesheet -> Result (Time, Duration)
timeThisShift t ts = resultFromMaybe "not clocked in" $ (\st -> (st, st ~~ t)) <$> start ts

timeThisPeriod :: Time -> Timesheet -> Duration
timeThisPeriod t ts = periodDuration (currentPeriod ts) + withDefault 0 (snd <$> timeThisShift t ts)

endPeriod :: Timesheet -> Result Timesheet
endPeriod ts | clockedIn ts = fail "can't end period while clocked in"
endPeriod ts = case currentPeriod ts of
  [] -> fail "can't end period with no shifts"
  p  -> return ts { currentPeriod = [], pastPeriods = pastPeriods ts +: p }

safeIndex :: Integer -> [a] -> Maybe a
safeIndex _ []        = Nothing
safeIndex i _ | i < 0 = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex i (_:xs)    = safeIndex (i - 1) xs

-- | 1-indexed from oldest to newest
getPeriod :: Integer -> Timesheet -> Result Period
getPeriod i ts = resultFromMaybe ("there is no period " ++ show i) $ safeIndex (i - 1) $ pastPeriods ts

periodRange :: Period -> Result (Time, Time)
periodRange [] = fail "can't determine range for empty period"
periodRange ss = return (minimum $ map fst ss, maximum $ map snd ss)