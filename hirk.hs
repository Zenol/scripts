#!/usr/bin/runhaskell

{-# LANGUAGE RankNTypes #-}


-- System :
import Network
import System.IO
import Text.Printf
import System.Exit
import System.Time
import System.Process

-- Web
import qualified Network.Curl as C
import Network.URI (isReserved, escapeURIString)
import Text.XML.HXT.Core

-- Data
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B

-- TYPES
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Exception.Base

data Bot = Bot
           { socket      :: Handle
           , startTime   :: ClockTime
           , danceState  :: Dance
           }

type BotSt = StateT Bot IO

data Dance = DLeft | DRight | DMiddleL | DMiddleR deriving (Show, Eq)

-- Usefull functions
botGetLine :: BotSt String
botGetLine = do
  h <- gets socket
  liftIO $ hGetLine h

-- Funny stuff
dance :: Bot -> String
dance b
  | danceState b == DLeft     = "<(^.^<)"
  | danceState b == DRight    = "(>^.^)>"
  | danceState b == DMiddleL  = "(>^.^<)"
  | danceState b == DMiddleR  = "<(^.^)>"

updateDance :: Bot -> Bot
updateDance b
  | danceState b == DLeft    = b {danceState = DMiddleL}
  | danceState b == DMiddleL = b {danceState = DRight}
  | danceState b == DRight   = b {danceState = DMiddleR}
  | danceState b == DMiddleR = b {danceState = DLeft}

----
---- Bot config
----

server    = "irc.langochat.fr"
port      = 6667
chan      = "#gcn"
nick      = "Hirk"
chrootdir = "/home/zenol/hirk_chroot/"

----
---- Main implementation
----

main :: IO ()
main = bracket connect disconnect runState
  where
    disconnect = hClose . socket
    runState = evalStateT run

-- | Create a Bot state
connect :: IO Bot
connect = do
  socket <- connectTo server (PortNumber. fromIntegral $ port)
  hSetBuffering socket NoBuffering
  time <- getClockTime
  return $ Bot socket time DLeft

-- Wait until the server says something containing the name of
-- the bot. It's to prevent some false positif virus from langochat.
waitForAnswer :: BotSt ()
waitForAnswer = do
  s <- botGetLine
  ping s
  liftIO $ putStrLn s
  case nick `isInfixOf` s of
    True  -> return ()
    False -> waitForAnswer

run :: BotSt ()
run = do
  write "NICK" nick
  write "USER" (nick++" 0 * : Zenol's Bot")
  waitForAnswer
  write "JOIN" chan
  listen


write :: String -> String -> BotSt ()
write s t = do
  h <- gets socket
  liftIO $ hPrintf h "%s %s\r\n" s t
  liftIO $ printf    "> %s %s\n" s t


listen :: BotSt ()
listen = do
  s <- botGetLine
  liftIO $ putStrLn s
  case ircSplit $ s of
    Just a -> process a
    _      -> ping s
  listen

ping :: String -> BotSt ()
ping s = case ("PING :" `isPrefixOf` s) of
  True  -> write "PONG" (':' : drop 6 s)
  False -> liftIO $ return ()

----
---- Parse an IRC message
----

data IrcCmd = Notice | Privmsg | Unknown

getCmd :: String -> IrcCmd
getCmd "NOTICE"  = Notice
getCmd "PRIVMSG" = Privmsg
getCmd _         = Unknown

data Event = Event
             { eNick    :: String
             , eUri     :: String
             , eIrcCmd  :: IrcCmd
             , eDest    :: String
             , eMessage :: String
             }

ircSplit :: String -> Maybe Event

ircSplit (':' : s) = result
  where
    result = case tail of
      '!' : s -> Just $ Event nick uri (getCmd cmdString) dest msg
      _       -> Nothing
    (nick, tail)        = break ('!'==) s
    (uri, tail')        = break (' '==) . drop 1 $ tail
    (cmdString, tail'') = break (' '==) . drop 1 $ tail'
    (dest, tail''')     = break (' '==) . drop 1 $ tail''
    msg                 = drop 1 . takeWhile ('\r'/=) . dropWhile (':'/=) $ tail'''

ircSplit _ = Nothing

----
---- React to messages
----

process :: Event -> BotSt ()
process e = case eNick e of
  -- Ignore SecureServ
  "SecureServ" -> return ()
  _            -> case eIrcCmd e of
    Notice  -> return ()
    --Only answer to Privmsg
    Privmsg -> processPrivmsg e
    _       -> return ()

processPrivmsg :: Event -> BotSt ()
processPrivmsg e = (mapM_ (sendMsg (answ e))) =<< (processCmd False (eMessage e) e)

-- y == true if it's a sub string of a command.
-- (used by !reverse to know we should return [s]
processCmd :: Bool -> String -> Event -> BotSt [String]
processCmd y s e
  | s == nick ++ "> Part loin et ne reviens jamais!" = do
    write "QUIT" ":\"Je vais me cacher...\""
    liftIO $ exitWith ExitSuccess
    return []
processCmd y s e
  | "!length " `isPrefixOf` s = do
    return [show . length . drop 8 $ s]
processCmd y s e
  | "!google " `isPrefixOf` s = do
    concatMapM (google e) =<< (processCmd True (drop (length "!google ") s) e)
processCmd y s e
  | "php> " `isPrefixOf` s = do
    runPhp (drop (length "php ") s)
processCmd y "!dance" e = do
    string <- gets dance
    modify updateDance
    return [string]
processCmd y "!help" e = do
  return ["Je connais !dance, !uptime, !time, !reverse, !google et !quit."]
processCmd y "!uptime" e = do
  now <- liftIO $ getClockTime
  start <- gets startTime
  return [displayTime $ diffClockTimes now start]
processCmd y s e
  | "tulas" `isInfixOf` s = do
    return ["Tulas à vous!"]
processCmd y "!time" e = do
  now <- liftIO $ toCalendarTime =<< getClockTime
  return [displayCalendar now]
processCmd y "!quit" e
  | (eNick e) == "Zenol" = do
    write "QUIT" ":\"Snif ...\""
    liftIO $ exitWith ExitSuccess
processCmd y s e
  | "!reverse " `isPrefixOf` s = do
    fmap (map utf8Reverse) $ processCmd True (drop 9 $ s) e
processCmd y s e = case eDest e of
    x | x == nick && y == False
                  -> return ["Je ne suis pas complètement implémenté :("]
    _             -> if y then return [s] else do
      liftIO $ putStr "Not interpreted : " >> putStrLn s
      return []

sendMsg :: String -> String -> BotSt ()
sendMsg nick msg = write "PRIVMSG" (nick ++ " :" ++ msg)


--- Usefull stuff

limit :: Int -> String -> String
limit n s = if length s > n then
              (take (n - 3) s) ++ "..."
            else
              s

utf8Reverse :: String -> String
utf8Reverse = B.unpack . T.encodeUtf8 . T.reverse . T.decodeUtf8 . B.pack

(<.>) :: T.Text -> T.Text -> T.Text
(<.>) = T.append

displayTime :: TimeDiff -> String
displayTime td = hour ++ " heures, " ++ min ++ " minuttes et " ++ sec ++ " secondes."
  where
    hour = show . tdHour $ td
    sec = show . tdSec $ td
    min = show . tdMin $ td

displayCalendar :: CalendarTime -> String
displayCalendar ct = day </> month </> year <-> hour <:> min <:> sec
  where
    (<:>) a b = a ++ ":" ++ b
    (</>) a b = a ++ "/" ++ b
    (<->) a b = a ++ " " ++ b
    day = show . ctDay $ ct
    month = show . fromEnum . ctMonth $ ct
    year = show . ctYear $ ct
    hour = show . ctHour $ ct
    sec = show . ctSec $ ct
    min = show . ctMin $ ct

answ e = case eDest e of
  x | x == nick -> eNick e
  _             -> chan

google :: Event -> String -> BotSt [String]
google e query = do
  (code, string) <- liftIO $ C.curlGetString
                    ("https://www.google.fr/search?q=" ++ (escapeURIString isReserved query)) []
  case code of
    C.CurlOK -> parseGoogle string
    _        -> do
      liftIO . putStrLn $ string
      return ["Query failed with " ++ (show code)]
  where
    parseGoogle string = do
      dtList <- liftIO $ ioDt
      return dtList
      where
        doc = readString [withParseHTML yes, withWarnings no] string
        ioDt = fmap (fmap format . take 3) $ runX $
             doc //> hasName "h3" /> hasName "a" >>>
             ((arr concat <<< listA (deep getText)) &&& getAttrValue "href")
        format (a, b) = (show a) ++ " : " ++ (takeWhile ('&'/=) . drop (length "/url?q=") $ b)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f


-- Run things
runPhp :: String -> BotSt ([String])
runPhp phpString = do
  let cmd  = "timelimit"
  let args = ["-t2",
              "timeout", "-sSIGKILL", "3",
              "safe_chroot", chrootdir,
              "timeout", "-sSIGKILL", "3",
              "/bin/php"
             ]
  let input = "<?php " ++ phpString
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args input
  let output = case stderr of
        "" -> lines $ case stdout of
                        "" -> "PHP executed."
                        _  -> stdout
        _  -> lines $ stderr
  -- Log command
  hirkLog "run_php" $ foldl (\a b -> a ++ " " ++ b) cmd args
  hirkLog "run_php" $ stdout ++ stderr
  -- Return output (limited to 3 lines)
  return $ take 3 . fmap (limit 100) $ output


-- Logs
hirkLog :: String -> String -> BotSt ()
hirkLog name message = do
  liftIO $ putStrLn $ name' ++ message'
  where
    name' = "<" ++ name ++ ">: "
    message' = message
