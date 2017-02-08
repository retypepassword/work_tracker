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

-- main = getArgs >>= putStr . work

-- readWorkFile :: IO Handle -> IO String
-- readWorkFile hdl = inEOF >>= testRead
--     where inEOF = hdl >>= hIsEOF
--           readWorkLine = hdl >>= hGetLine
--           testRead in_eof = if in_eof then return "" else readWorkLine
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


tellSuccess :: Bool -> String
tellSuccess True = "Success!"
tellSuccess False = "That didn't work."

work :: [String] -> IO String
work ["start"] = liftM tellSuccess $ liftM isRight $ writeWorkFile "start" openWorkFile
work ["stop"] = liftM tellSuccess $ liftM isRight $ writeWorkFile "stop" openWorkFile
-- work ["total", "today"] = 
-- work ["total"] =

exit = exitWith ExitSuccess
die  = exitWith (ExitFailure 1)
