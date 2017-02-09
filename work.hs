import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Data.Either
import GHC.IO.Handle
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Control.Monad

main = getArgs >>= work >>= putStr . (++ "\n")

data Status = Start | Stop deriving Show
convertStatus Start = True
convertStatus Stop = False
convertToStatus "start" = Start
convertToStatus "stop" = Stop

type StartTime = Int
type TotalTime = Int

readWorkFile :: IO String
readWorkFile = readFile "work_log"

writeWorkFile :: String -> IO (Either IOError ())
writeWorkFile action = tryIOError $ written >>= write
    where curTime = getCurrentTime >>= return . formatTime defaultTimeLocale "%s"
          written = curTime >>= return . (++) (action ++ " ") >>= return . (++ "\n")
          write string = appendFile "work_log" string

sumSecsToMin :: Double -> Double
sumSecsToMin = (/ 60)

getDay :: String -> IO Int
getDay day = (liftM (formatTime defaultTimeLocale "%j") $ parsedLocalTime) >>= dayToInt
    where localTime tz = utcToLocalTime tz ((parseTimeOrError False defaultTimeLocale "%s" day :: UTCTime))
          parsedLocalTime = getCurrentTimeZone >>= return . localTime 
          dayToInt = return . read

getSecs :: String -> UTCTime
getSecs day = parseTimeOrError False defaultTimeLocale "%s" day

tellSuccess :: (Either IOError ()) -> String
tellSuccess (Right ()) = "Success!"
tellSuccess (Left err) = "Error: " ++ (show $ ioeGetErrorType err)

splitStringFor2 :: (a -> [String] -> a) -> a -> String -> a
splitStringFor2 f tup str = f tup (words str)

splitStringFor1 :: ([String] -> a) -> String -> a
splitStringFor1 f str = f (words str)

sumTimes :: (TotalTime, StartTime, Status) -> [String] -> (TotalTime, StartTime, Status)
-- If the previous action was stop, then this one is probably start
sumTimes (total, _, Stop) [status, time] = (total, (read time) :: Int, convertToStatus status)
-- Vice versa
sumTimes (total, start, Start) [status, time] = (total + ((read time) :: Int) - start, 0, convertToStatus status)

showTotal :: [String] -> String
showTotal = show . (\(x, _, _) -> x) . (foldl (splitStringFor2 sumTimes) (0, 0, Stop))

justToday :: [String] -> IO Bool
justToday ["start", time] = getDay time >>= isToday
    where isToday day = getCurrentTime >>= return . formatTime defaultTimeLocale "%s" >>= getDay >>= return . (== day)
justToday [_, _] = return False

work :: [String] -> IO String
work ["start"] = liftM tellSuccess $ writeWorkFile "start"
work ["stop"] = liftM tellSuccess $ writeWorkFile "stop"
work ["total", "today"] = readWorkFile
    >>= filterM (splitStringFor1 justToday) . lines >>= return . showTotal
work ["total"] = readWorkFile >>= return . showTotal . lines
work _ = return $ "Usage:\n" ++
    "Run... work start:       to indicate that you've started working\n" ++
    "       work stop:        to indicate that you've stopped working\n" ++
    "       work total today: to figure out how much time you've worked today\n" ++
    "       work total:       to figure out how much time you've worked overall"
