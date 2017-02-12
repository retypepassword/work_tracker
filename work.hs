import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Data.Either
import Data.List
import Data.Function
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

divBy60 :: Int -> Int
divBy60 = floor . (/ 60) . fromIntegral

toHrMin :: Int -> String
toHrMin = concat . concat . transpose . (flip (++) [[" hr, ", " min"]]) . (: []) . map show . uncurry ((++) `on` (: [])) . (`quotRem` 60) . divBy60

getDay :: String -> IO Int
getDay day = (liftM (formatTime defaultTimeLocale "%j") $ parsedLocalTime) >>= dayToInt
    where localTime tz = utcToLocalTime tz ((parseTimeOrError False defaultTimeLocale "%s" day :: UTCTime))
          parsedLocalTime = getCurrentTimeZone >>= return . localTime 
          dayToInt = return . read

getSecs :: String -> UTCTime
getSecs = parseTimeOrError False defaultTimeLocale "%s"

tellSuccess :: (Either IOError ()) -> String
tellSuccess (Right ()) = "Success!"
tellSuccess (Left err) = "Error: " ++ (show $ ioeGetErrorType err)

splitStringFor2 :: (a -> [String] -> a) -> a -> String -> a
splitStringFor2 = flip . splitStringFor . flip

splitStringFor :: ([String] -> a) -> String -> a
splitStringFor f str = f (words str)

sumTimes :: (TotalTime, StartTime, Status) -> [String] -> (TotalTime, StartTime, Status)
-- If the previous action was stop, then this one is probably start
sumTimes (total, _, Stop) [status, time] = (total, (read time) :: Int, convertToStatus status)
-- Vice versa
sumTimes (total, start, Start) [status, time] = (total + ((read time) :: Int) - start, 0, convertToStatus status)

showTotal :: [String] -> String
showTotal = toHrMin . (\(x, _, _) -> x) . (foldl (splitStringFor2 sumTimes) (0, 0, Stop))

justToday :: [String] -> IO Bool
justToday [_, time] = getDay time >>= isToday
    where isToday day = getCurrentTime >>= return . formatTime defaultTimeLocale "%s" >>= getDay >>= return . (== day)

work :: [String] -> IO String
work ["start"] = liftM tellSuccess $ writeWorkFile "start"
work ["stop"] = liftM tellSuccess $ writeWorkFile "stop"
work ["total", "today"] = readWorkFile
    >>= filterM (splitStringFor justToday) . lines >>= return . showTotal
work ["total"] = readWorkFile >>= return . showTotal . lines
work _ = return $ "Usage:\n" ++
    "Run... work start:       to indicate that you've started working\n" ++
    "       work stop:        to indicate that you've stopped working\n" ++
    "       work total today: to figure out how much time you've worked today\n" ++
    "       work total:       to figure out how much time you've worked overall"
