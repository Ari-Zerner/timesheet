module Main where

import Lib
import Result
import System.Console.StructuredCLI
import Control.Monad.State.Lazy
import Data.Default (def)
import Data.Time.Format
import Data.Time ( utcToLocalTime, LocalTime, TimeZone, getCurrentTimeZone, utc )
import Text.Read (readMaybe)
import Data.Maybe

data CLIState = CLIState { timesheet :: Timesheet
                         , selection :: Maybe Integer
                         , timezone  :: TimeZone
                         }

type CLI = StateT CLIState IO
type Command = CommandsT CLI ()

selectedPeriod :: CLIState -> Period
selectedPeriod state = fromMaybe (currentPeriod $ timesheet state) $ selection state >>= maybeFromResult . flip getPeriod (timesheet state)

localize :: CLIState -> Time -> LocalTime
localize = utcToLocalTime . timezone

showTime :: CLIState -> Time -> String
showTime state = formatTime defaultTimeLocale "%-H:%M %a %b %-e" . localize state

showDate :: CLIState -> Time -> String
showDate state = formatTime defaultTimeLocale "%F" . localize state

showShift :: CLIState -> Shift -> String
showShift state (start, end) = showTime state start ++ " - " ++ showTime state end ++ " (" ++ show (durationToHours $ start ~~ end) ++ ")"

showPeriod :: CLIState -> Period -> String
showPeriod state period = case periodRange period of
  Failure _            -> "Empty period (0 hours)"
  Success (start, end) -> showDate state start ++ " - " ++ showDate state end ++ " (" ++ details ++ ")"
    where details = show (length period) ++ " shift" ++ plural ++ ", " ++ show (durationToHours $ periodDuration period)
          plural  = if length period == 1 then "" else "s"

getTime :: CLI Time
getTime = lift getCurrentTime

modifyTimesheet :: (Timesheet -> Result Timesheet) -> CLI ()
modifyTimesheet f = do
  ts <- gets timesheet
  ts' <- lift $ forceResult $ f ts
  modify $ \state -> state { timesheet = ts' }

ifClockedIn :: CLI Bool
ifClockedIn = gets $ clockedIn . timesheet

ifClockedOut :: CLI Bool
ifClockedOut = not <$> ifClockedIn

ifCurrent :: CLI Bool
ifCurrent = gets $ isNothing . selection

(&&&) :: Monad m => m Bool -> m Bool -> m Bool
(&&&) = liftM2 (&&)

clockin :: Command
clockin = command' "clockin" "Start a shift" (ifCurrent &&& ifClockedOut) $ do
  t <- getTime
  modifyTimesheet $ clockIn t
  state <- get
  lift $ putStrLn $ "Clocked in at " ++ showTime state t ++ "."
  noAction

clockout :: Command
clockout = command' "clockout" "End the current shift" (ifCurrent &&& ifClockedIn) $ do
  t <- getTime
  state <- get
  (ts, shiftLength) <- lift $ forceResult $ clockOut t $ timesheet state
  put state { timesheet = ts }
  lift $ putStrLn $ "Clocked out at " ++ showTime state t ++ ". Shift length: " ++ show (durationToHours shiftLength) ++ "."
  noAction

shift :: Command
shift = command' "shift" "See details about the current shift" (ifCurrent &&& ifClockedIn) $ do
  t <- getTime
  state <- get
  (start, hours) <- lift $ forceResult $ timeThisShift t $ timesheet state
  lift $ putStrLn $ "Clocked in for " ++ show (durationToHours hours) ++ ", since " ++ showTime state start
  noAction

shifts :: Command
shifts = command "shifts" "List completed shifts this period" $ do
  state <- get
  let completed = selectedPeriod state
  lift $ do
    putStrLn $ showPeriod state completed
    forM_ completed $ putStrLn . showShift state
  noAction

endperiod :: Command
endperiod = command' "endperiod" "End the current period" (ifCurrent &&& gets (isSuccess . endPeriod . timesheet)) $ do
  modifyTimesheet endPeriod
  noAction

periods :: Command
periods = command "periods" "List completed periods" $ do
  state <- get
  let completed = zip [1..] $ pastPeriods $ timesheet state
  lift $ if null completed
    then putStrLn "No completed periods."
    else forM_ completed $ \(i, period) -> putStrLn (show i ++ ": " ++ showPeriod state period)
  noAction

changePeriod :: Handler CLI (Maybe Integer -> Maybe Integer)
changePeriod f = do
  state <- get
  let newSelection = mfilter (isSuccess . flip getPeriod (timesheet state)) $ f $ selection state
  put state {selection = newSelection}
  lift $ putStrLn $ case newSelection of
    Nothing -> "Now viewing current period."
    Just i  -> "Now viewing period " ++ show i ++ "."
  noAction

period :: Command
period = param "period" "<i> – View period i" validator (changePeriod . const . Just)
  where validator :: Validator CLI Integer
        validator arg = return $ readMaybe arg

current :: Command
current = command' "current" "Return to the current period" (gets $ isJust . selection) $ changePeriod $ const Nothing

previous :: Command
previous = command' "previous" "Go to the previous period" (gets condition) $ do
  last <- gets $ length . pastPeriods . timesheet
  changePeriod $ Just . maybe (toInteger last) (subtract 1)
  where condition state = selection state /= Just 1 && (not . null . pastPeriods . timesheet) state

next :: Command
next = command' "next" "Go to the next period" (gets $ isJust . selection) $ do
  last <- gets $ length . pastPeriods . timesheet
  changePeriod $ mfilter (<= toInteger last) . fmap (+ 1)

quit :: Command
quit = command "^C" "Quit" $ lift (putStrLn "To quit, press control-C.") >> noAction

root :: Command
root = do -- command order gets reversed
  previous
  next
  current
  period
  periods
  endperiod
  shifts
  shift
  clockout
  clockin
  quit

main :: IO ()
main = evalStateT (void $ runCLI "" set root) emptyCLIState
  where emptyCLIState = CLIState { timesheet = blankTimesheet
                                 , selection = Nothing
                                 , timezone  = utc
                                 }
        set = def { getBanner = "Welcome to Timesheet! Type ? to show commands." 
                  , getPrompt = do
                      tz <- lift getCurrentTimeZone
                      modify $ \state -> state {timezone = tz}
                      state <- get
                      let ts = timesheet state
                      current <- lift $ case selection state of
                        Nothing -> do
                          today <- getCurrentTime
                          let (start, _) = withDefault (today, today) $ periodRange $ currentPeriod ts
                          return $ "current period" ++ " (started " ++ showDate state start ++ ")"
                        Just i -> do
                          (start, end) <- forceResult $ getPeriod i ts >>= periodRange
                          return $ "period " ++ show i ++ " (" ++ showDate state start ++ " to " ++ showDate state end ++ ")"
                      let inOut = if clockedIn ts then "in" else "out"
                      return $ "\nClocked " ++ inOut ++ ", viewing " ++ current ++ ".\n\n> "
                  -- TODO exception handler?
                  }
