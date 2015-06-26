module Processor
where

import Data.Time

import Parser

defaultTime :: UTCTime
defaultTime = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

data Line = Line UTCTime Event
          | Snip

type Nick     = String
type Hostname = String

data Event = Message Nick String
           | Action  Nick String
           | DateChange
           | Join    Nick Hostname
           | Quit    Nick Hostname String
           | NickChange Nick Nick
           | NoNick Nick
           | Censored
           | PMReceive Nick String
           | PMSend Nick String
           | LogClose
           | LogOpen
           | Mode Nick String Hostname

processRawLines :: [(RawLine, Int)] -> ([Line], [Warning])
processRawLines rl = (reverse lines, reverse $ ircWarnings st')
    where
        processFun (ls, st) rl = case processRawLine st rl of
            (newLs, st') -> (newLs ++ ls, st')
        (lines, st') = foldl processFun ([], defaultIRCState) rl

data PMState = PMNone | PMIsSend | PMIsReceive

data IRCState = IRCState { ircTime       :: UTCTime
                         , ircSpeaker    :: Nick
                         , ircPMState    :: PMState
                         , ircWasReceive :: Bool
                         , ircWarnings   :: [Warning] }

defaultIRCState :: IRCState
defaultIRCState = IRCState defaultTime "" PMNone False []

processRawLine :: IRCState -> (RawLine, Int) -> ([Line], IRCState)
processRawLine st (rl, ln) = case rl of
    RAction str  -> ([Line (ircTime st) $ Action  (ircSpeaker st) str], st)
    RDate mo d y -> case compare oldTime newTime of
        LT -> ([Line newTime DateChange], st { ircTime = newTime } )
        EQ -> ([], st)
        GT -> ([Line newTime DateChange], st { ircTime = newTime, ircWarnings = TimeTravelWarning ln : ircWarnings st } )
        where oldTime   = ircTime st
              newTime   = UTCTime (fromGregorian y mo d) (secondsToDiffTime 0)
    RText   str  -> ([Line (ircTime st) $ event (ircSpeaker st) str], st)
        where event = case ircPMState st of
                  PMNone      -> Message
                  PMIsReceive -> PMReceive
                  PMIsSend    -> PMSend
    RNick str    -> ([], st { ircSpeaker = str, ircPMState = PMNone } )
    RTime h m pm -> (if (dayTime' - dayTime) > 0 then [] else [Line newTime DateChange], st { ircTime = newTime } )
        where oldTime@(UTCTime day dayTime) = ircTime st
              dayTime'  = secondsToDiffTime $ fromIntegral (3600 * h + 60 * m + if pm then 12 * 3600 else 0)
              newTime   = if (dayTime' - dayTime) > 0
                  then UTCTime day dayTime'
                  else UTCTime (addDays 1 day) dayTime'
    RRelTime int -> (if dayDiff == 0 then [] else [Line newTime DateChange], st { ircTime = newTime } )
        where oldTime = ircTime st
              newTime = addUTCTime (fromIntegral $ int * 60) oldTime
              dayDiff = diffDays (utctDay newTime) (utctDay oldTime)
    RCommand cmd -> case cmd of
        CJoin nick       -> ([Line (ircTime st) $ Join nick "unknown@unknown"], st)
        CQuit nick msg   -> ([Line (ircTime st) $ Quit nick "unknown@unknown" msg], st)
        CNick nick nick' -> ([Line (ircTime st) $ NickChange nick nick'], st)
        CNoNick nick     -> ([Line (ircTime st) $ NoNick nick], st)
        CCensored        -> ([Line (ircTime st)   Censored], st)
        CMode ni mod hos -> ([Line (ircTime st) $ Mode ni mod hos], st)
        -- TODO: hostnames
    RPMReceive n -> ([], st { ircSpeaker = n, ircPMState = PMIsReceive } )
    RPMSend n    -> ([], st { ircSpeaker = n, ircPMState = PMIsSend } )
    RSnip        -> ([Snip], st)
    RSystem str  -> ([], st) -- TODO
    REmpty       -> ([], st)
    RComment _   -> ([], st)

data Warning = TimeTravelWarning Int
