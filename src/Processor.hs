module Processor
where

import Data.Time

import Parser

-- log_close_string = --- Log closed %a %b %d %H:%M:%S %Y
-- log_timestamp    = %H:%M
-- log_day_changed  = --- Day changed %a %b %d %Y
-- log_open_string  = --- Log opened %a %b %d %H:%M:%S %Y

defaultTime :: UTCTime
defaultTime = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

data Line = Line UTCTime Event
          | Snip

data Event = Message Nick String
           | Action  Nick String
           | DateChange
           | Join    Nick Hostname
           | Quit    Nick Hostname String
           | NickChange Nick Nick
           | Censored
           | PMReceive Nick String
           | PMSend Nick String
           | LogClose
           | LogOpen

type Nick     = String
type Hostname = String

processRawLines :: [(RawLine, Int)] -> ([Line], [Warning])
processRawLines rl = (reverse lines, reverse $ ircWarnings st')
    where
        processFun (ls, st) rl = case processRawLine st rl of
            (newLs, st') -> (newLs ++ ls, st')
        (lines, st') = foldl processFun ([], defaultIRCState) rl

data IRCState = IRCState { ircTime       :: UTCTime
                         , ircSpeaker    :: Nick
                         , ircPmNick     :: Nick
                         , ircWasReceive :: Bool
                         , ircWarnings   :: [Warning] }

defaultIRCState :: IRCState
defaultIRCState = IRCState defaultTime "" "" False []

processRawLine :: IRCState -> (RawLine, Int) -> ([Line], IRCState)
processRawLine st (rl, ln) = case rl of
    RAction str  -> ([Line (ircTime st) $ Action  (ircSpeaker st) str], st)
    RDate mo d y -> case compare oldTime newTime of
        LT -> ([Line newTime DateChange], st { ircTime = newTime } )
        EQ -> ([], st)
        GT -> ([Line newTime DateChange], st { ircTime = newTime, ircWarnings = TimeTravelWarning ln : ircWarnings st } )
        where oldTime   = ircTime st
              newTime   = UTCTime (fromGregorian y mo d) (secondsToDiffTime 0)
    RText   str  -> ([Line (ircTime st) $ Message (ircSpeaker st) str], st)
    RCommand str -> ([], st) -- TODO
    RNick str    -> ([], st) -- TODO
    RPMReceive str -> ([], st) -- TODO
    RPMSend str  -> ([], st) -- TODO
    RRelTime int -> ([], st) -- TODO
    RSnip        -> ([Snip], st)
    RSystem str  -> ([], st) -- TODO
    RTime h m pm -> ([], st) -- TODO
    REmpty       -> ([], st)
    RComment _   -> ([], st)

data Warning = TimeTravelWarning Int
