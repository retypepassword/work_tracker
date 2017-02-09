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

main = getArgs >>= work

data Status = Start | Stop deriving Show
convertStatus Start = True
convertStatus Stop = False
convertToStatus "start" = Start
convertToStatus "stop" = Stop

type StartTime = Int
type TotalTime = Int

readWorkFile :: IO String
readWorkFile = readFile "workLog"

openWorkFile :: IO Handle
openWorkFile = openFile "workLog" AppendMode

writeWorkFile :: String -> IO Handle -> IO (Either IOError ())
writeWorkFile action hdl = tryIOError $ written >>= write hdl
    where curTime = getCurrentTime >>= return . formatTime defaultTimeLocale "%s"
          written = curTime >>= return . (++) (action ++ " ")
          write handle string = handle >>= (\handle' -> hPutStr handle' string)

-- getSecs :: String -> String -> Int

sumSecsToMin :: Double -> Double
sumSecsToMin = (/ 60)

getDay :: String -> IO Int
getDay day = (liftM (formatTime defaultTimeLocale "%j") $ parsedLocalTime) >>= dayToInt
    where localTime tz = utcToLocalTime tz ((parseTimeOrError False defaultTimeLocale "%s" day :: UTCTime))
          parsedLocalTime = getCurrentTimeZone >>= return . localTime 
          dayToInt = return . read

getSecs :: String -> UTCTime
getSecs day = parseTimeOrError False defaultTimeLocale "%s" day

tellSuccess :: Bool -> String
tellSuccess True = "Success!"
tellSuccess False = "That didn't work."

splitStringFor2 :: ([String] -> a -> a) -> String -> a -> a
splitStringFor2 f str tup = f (words str) tup

splitStringFor1 :: ([String] -> a) -> String -> a
splitStringFor1 f str = f (words str)

sumTimes :: [String] -> (TotalTime, StartTime, Status) -> (TotalTime, StartTime, Status)
sumTimes [status, time] (total, _, Start) = (total, (read time) :: Int, convertToStatus status)
sumTimes [status, time] (total, start, Stop) = (total + ((read time) :: Int) - start, 0, convertToStatus status)

showTotal :: [String] -> String
showTotal = show . (\(x, _, _) -> x) . (foldr (splitStringFor2 sumTimes) (0, 0, Stop))

justToday :: [String] -> IO Bool
justToday ["start", time] = getDay time >>= isToday
    where isToday day = getCurrentTime >>= return . formatTime defaultTimeLocale "%s" >>= getDay >>= return . (== day)
justToday [_, _] = return False

work :: [String] -> IO String
work ["start"] = liftM tellSuccess $ liftM isRight $ writeWorkFile "start" openWorkFile
work ["stop"] = liftM tellSuccess $ liftM isRight $ writeWorkFile "stop" openWorkFile
work ["total", "today"] = readWorkFile
    >>= filterM (splitStringFor1 justToday) . lines >>= return . showTotal
work ["total"] = readWorkFile >>= return . showTotal . lines
