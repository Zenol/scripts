#!/usr/bin/runhaskell

-- System :
import Network
import System.IO
import Text.Printf
import System.Exit
import System.Time
import System.Process
import System.Locale (defaultTimeLocale)

-- Web
import qualified Network.Curl as C
import Network.URI (isReserved, escapeURIString)
import Text.XML.HXT.Core

-- Data
import Data.Maybe (maybe, fromMaybe)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)

-- TYPES
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Exception.Base

----
---- Bot config
----
server    = "irc.langochat.fr"
port      = 6667
chan      = "#gcn"
nick      = "Hirk"
chrootdir = "/home/zenol/hirk_chroot/"
logFile   = "hirk.log"

-- The Bot Monad --
type BotSt = StateT Bot IO

data Bot = Bot
           { socket      :: Handle
           , startTime   :: ClockTime
           , danceState  :: Dance
           }

botGetLine :: BotSt String
botGetLine = do
  h <- gets socket
  liftIO $ hGetLine h

-- Funny stuff
data Dance = DLeft | DRight | DMiddleL | DMiddleR deriving (Show, Eq)

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

------------------------------
---- IRC Connection Stuff ----
------------------------------

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

-- | The true entry point of the bot :)
run :: BotSt ()
run = do
  write "NICK" nick
  write "USER" (nick++" 0 * : Zenol's Bot")
  waitForAnswer
  write "JOIN" chan
  listen

-- | Write an IRC message
write :: String -> String -> BotSt ()
write s t = do
  h <- gets socket
  liftIO $ hPrintf h "%s %s\r\n" s t
  hirkLog "output" $ printf "%s %s\n" s t

-- | Listen for an IRC message. Display it and give it to 'process'
listen :: BotSt ()
listen = do
  s <- botGetLine
  liftIO $ putStrLn s
  maybe (ping s) process (ircSplit s)
  listen

-- | Answer to a ping message if s is a PING query
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

-- | Ignore SecureServ, call processPrivmsg on Privmsg
process :: Event -> BotSt ()
process e = case eNick e of
  -- Ignore SecureServ
  "SecureServ" -> return ()
  _            -> case eIrcCmd e of
    Notice  -> return ()
    --Only answer to Privmsg
    Privmsg -> processPrivmsg e
    _       -> return ()

-- | Call processCmd on the event, and send the output to IRC
processPrivmsg :: Event -> BotSt ()
processPrivmsg e = (mapM_ . sendMsg $ answ e) =<< (processCmd False s e)
  where
    s = strip . rstrip . eMessage $ e

------------------------------
-- Command processing stuff --
------------------------------
type Message = String

data LookingWay = Prefix | Infix | Suffix | Equal
type CommandName = String
type Job = Message -> Event -> BotSt [String]

data Command = Command
               { commandName :: CommandName
               , lookingWay  :: LookingWay
               , job         :: Job
               }

-- | Fancy operator (It's only for the beautiful notation)
(~:) :: (a -> b) -> a -> b
(~:) = ($)
infixl 0 ~:

-- | List of
commandList :: [Command]
commandList =
  [ Command ~: "!length"
            ~: Prefix
            ~: \s e -> fmap . fmap ~: show . length ~: processCmd True s e
  , Command ~: "!reverse"
            ~: Prefix
            ~: \s e -> fmap (map utf8Reverse) $ processCmd True s e
  , Command ~: "!google"
            ~: Prefix
            ~: \s e -> concatMapM (google e) =<< (processCmd True s e)
  , Command ~: "!dance"
            ~: Prefix
            ~: \_ _ -> gets dance >>= (\x -> modify updateDance >> return [x])
  , Command ~: "!uptime"
            ~: Equal
            ~: uptime
  , Command ~: "!uptime"
            ~: Infix
            ~: \_ _ -> return ["Tulas à vous"]
  , Command ~: "!bestwebsite"
            ~: Equal
            ~: \_ _ -> return ["http://zenol.fr biensur!"]
  , Command ~:  "!quit"
            ~: Equal
            ~: \s e -> if (eNick e) == "Zenol" then quit "Snif ..." s e else return []
  , Command ~: "!time"
            ~: Equal
            ~: \_ e -> liftIO $ fmap ((: []) . displayCalendar) $ toCalendarTime =<< getClockTime
  , Command ~: "!help"
            ~: Equal
            ~: help

  , Command ~: "php>"
            ~: Prefix
            ~: \s _ -> runPhp s
  , Command ~: "lua>"
            ~: Prefix
            ~: \s _ -> runLua s
  , Command ~: "python>"
            ~: Prefix
            ~: \s _ -> runPython s
  , Command ~: "perl>"
            ~: Prefix
            ~: \s _ -> runPerl s
  , Command ~: "ruby>"
            ~: Prefix
            ~: \s _ -> runRuby s
  , Command ~: "c>"
            ~: Prefix
            ~: \s _ -> runC s

  , Command ~: (nick ++ "> Part loin et ne reviens jamais!")
            ~: Equal
            ~: quit "Je vais me cacher..."
  ]

-- Process a command with the consumed input s, the event e, and
-- y == True if the untreated substring should be returned as output.
processCmd :: Bool -> String -> Event -> BotSt [String]
processCmd y s e = do
  case take 1 . filter selectCommand $ commandList of
    []    -> notInterpreted y s' e
    a : _ -> runHirkCommand a s' e
  where
    s' = strip s
    selectCommand (Command name op _) = selectOp op ~: name ~: s'
    selectOp Prefix = isPrefixOf
    selectOp Infix = isInfixOf
    selectOp Equal = (==)
    selectOp Suffix = isSuffixOf

-- | What happen to all non interpreted string (returned if y == True)
notInterpreted :: Bool -> String -> Event -> BotSt [String]
notInterpreted y s e = do
  case eDest e of
    x | x == nick && y == False
        -> return ["Je ne suis pas complètement implémenté :("]
    _   -> if y then return [s] else do
             hirkLog "not_interpreted" s
             return []

-- |  Apply the Command to the input, consuming prefix/suffix
runHirkCommand :: Command -> String -> Event -> BotSt [String]
runHirkCommand (Command cmd lw c) s e = case lw of
  Prefix -> c ~: drop (length cmd) s ~: e
  Suffix -> c (reverse . drop (length cmd) . reverse $ s) e
  _      -> c s e

--
-- Command functions
--

quit :: String -> Message -> Event -> BotSt [String]
quit quitMsg _ _ = do
  write "QUIT" (":\"" ++ quitMsg ++ "\"")
  liftIO $ exitWith ExitSuccess
  return []

uptime :: Message -> Event -> BotSt [String]
uptime _ _ = do
  now <- liftIO $ getClockTime
  start <- gets startTime
  return [displayTime $ diffClockTimes now start]

sendMsg :: String -> String -> BotSt ()
sendMsg nick msg = write "PRIVMSG" (nick ++ " :" ++ msg)

help :: Message -> Event -> BotSt [String]
help s e = return . (: []) $ foldl f "Je connais : " commandList
  where
    f = (\s (Command n _ _) -> s <-/-> n)

--- Usefull stuff
(<:>) a b = a ++ ":" ++ b
(</>) a b = a ++ "/" ++ b
(<-/->) a b = a ++ " / " ++ b
(<->) a b = a ++ " " ++ b

strip :: String -> String
strip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . strip . reverse

limit :: Int -> String -> String
limit n s = if length s > n then
              (take (n - 3) s) ++ "..."
            else
              s

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

utf8Reverse :: String -> String
utf8Reverse = B.unpack . T.encodeUtf8 . T.reverse . T.decodeUtf8 . B.pack

displayTime :: TimeDiff -> String
displayTime td = formatTimeDiff  defaultTimeLocale format td'
  where
    format = "%H heures, %M minuttes et %S secondes."
    td' = normalizeTimeDiff td

displayCalendar :: CalendarTime -> String
displayCalendar ct = day </> month </> year <-> hour <:> min <:> sec
  where
    day = show . ctDay $ ct
    month = show . fromEnum . ctMonth $ ct
    year = show . ctYear $ ct
    hour = show . ctHour $ ct
    sec = show . ctSec $ ct
    min = show . ctMin $ ct

-- | Select the good nickname / channel to answer to
answ e = case eDest e of
  x | x == nick -> eNick e
  _             -> chan

-- | Ask google for the 3 best results !!!
google :: Event -> String -> BotSt [String]
google e query = do
  (code, string) <- liftIO $ C.curlGetString
                    ("https://www.google.fr/search?q=" ++ (escapeURIString isReserved query)) []
  case code of
    C.CurlOK -> parseGoogle string
    _        -> do
      hirkLog "google" $ string
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

---------------------------------------------
-- Run interpretors for perl, php, lua.... --
---------------------------------------------

type Language = String
type Interpretor = String
runScript :: Language -> Interpretor -> String -> BotSt ([String])
runScript language interpretor scriptString = do
  let cmd  = "timelimit"
  let args = ["-t1",
              "timeout", "-sSIGKILL", "1.1",
              "safe_chroot", chrootdir,
              "timeout", "-sSIGKILL", "1.1",
              "env", "LANG=C",
              interpretor
             ]
  let input = scriptString
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode cmd args input
  let output = case stderr of
        "" -> lines $ case stdout of
                        "" -> language ++ " executed."
                        _  -> stdout
        _  -> lines $ case stderr of
              _ | "timelimit:" `isPrefixOf` stderr
                  -> "PHP timed out..."
              _   -> stderr
  -- Log command
  hirkLog ("run_" ++ language) $ foldl (\a b -> a ++ " " ++ b) cmd args -- query
  hirkLog ("run_" ++ language) $ (take 300 stdout) ++ (take 300 stderr) -- out/err
  -- Return output (limited to 3 lines)
  return $ take 3 . fmap (limit 100) $ output

runPhp :: String -> BotSt ([String])
runPhp = runScript "PHP" "/bin/php" . (++) "<?php "

runLua :: String -> BotSt ([String])
runLua = runScript "Lua" "/bin/lua"

runPerl :: String -> BotSt ([String])
runPerl = runScript "Perl" "/bin/perl"

runRuby :: String -> BotSt ([String])
runRuby = runScript "Ruby" "/bin/ruby"

runC :: String -> BotSt ([String])
runC = runScript "C" "/bin/picoc"

runPython :: String -> BotSt ([String])
runPython = runScript "Python" "/bin/python3"

-- | Log the input onto the screen and into logFile
hirkLog :: String -> String -> BotSt ()
hirkLog name message = do
  liftIO $ appendFile logFile output
  liftIO $ putStr output
  where
    name' = "<" ++ name ++ ">: "
    message' = message
    output = (++ "\n") $ rstrip (name' ++ message' ++ "\n")
