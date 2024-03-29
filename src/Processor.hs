module Processor
where

import Parser

import Control.Applicative
import Control.Monad.State
import Data.Time

defaultTime :: UTCTime
defaultTime = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

data Line = Line UTCTime Event
          | Raw String
          | BeginRaw
          | EndRaw
          | Snip deriving (Show)

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
           | Mode Nick String Hostname deriving (Show)

processRawLineBatches :: [[(RawLine, Int)]] -> ([Line], [Warning])
processRawLineBatches rlss = (concat . map reverse $ lss, reverse $ ircWarnings st')
    where
        processedBatches :: State IRCState [[Line]]
        processedBatches = sequence (map (foldM processFun []) rlss)
        (lss, st') = runState processedBatches defaultIRCState
        ls = concat . map reverse $ lss

processRawLines :: [(RawLine, Int)] -> ([Line], [Warning])
processRawLines rls = (reverse ls, reverse $ ircWarnings st')
    where
        (ls, st') = runState (processRls rls) defaultIRCState

processFun :: [Line] -> (RawLine, Int) -> State IRCState [Line]
processFun lns rawl = do
    newLs <- processRawLine rawl
    return (newLs ++ lns)

processRls :: [(RawLine, Int)] -> State IRCState [Line] 
processRls rls = do
    lns <- foldM processFun [] rls
    isRaw <- gets ircIsRaw
    if not isRaw
        then return (EndRaw : lns)
        else return lns

data PMState = PMNone | PMIsSend | PMIsReceive

data IRCState = IRCState { ircTime       :: UTCTime
                         , ircSpeaker    :: Nick
                         , ircPMState    :: PMState
                         , ircWasReceive :: Bool
                         , ircWarnings   :: [Warning]
                         , ircIsRaw      :: Bool }

defaultIRCState :: IRCState
defaultIRCState = IRCState defaultTime "" PMNone False [] True

modifyTime :: UTCTime -> State IRCState ()
modifyTime newTime = modify (\st -> st { ircTime = newTime } )

processRawLine :: (RawLine, Int) -> State IRCState [Line]
processRawLine (rl, ln) = do
    time <- gets ircTime
    speaker <- gets ircSpeaker
    pmState <- gets ircPMState
    isRaw <- gets ircIsRaw
    rawMod <- case rl of
        RRaw _ -> if isRaw
            then return []
            else modify (\st -> st { ircIsRaw = True } ) >> return [EndRaw] -- BUG: this isn't really showing up
        _      -> if not isRaw
            then return []
            else modify (\st -> st { ircIsRaw = False } ) >> return [BeginRaw]
    ls <- case rl of
        RAction str -> (: []) <$> ((flip Line . flip Action str) <$> gets ircSpeaker <*> gets ircTime)
        RDate mo d y -> case compare time newTime of
                LT -> do
                    modifyTime newTime
                    return [Line newTime DateChange]
                EQ -> return []
                GT -> do
                    modifyTime newTime
                    modify (\st -> st { ircWarnings = TimeTravelWarning ln : ircWarnings st } )
                    return [Line newTime DateChange]
            where newTime   = UTCTime (fromGregorian y mo d) (secondsToDiffTime 0)
        RText str  -> return [Line time $ event speaker str]
            where event = case pmState of
                      PMNone      -> Message
                      PMIsReceive -> PMReceive
                      PMIsSend    -> PMSend
        RNick str    -> do
            modify (\st -> st { ircSpeaker = str, ircPMState = PMNone } )
            return []
        RTime h m pm -> do
            modify (\st -> st { ircTime = newTime } )
            return (if (dayTime' - dayTime) > 0 then [] else [Line newTime DateChange])
            where (UTCTime day dayTime) = time
                  dayTime'  = secondsToDiffTime $ fromIntegral (3600 * h + 60 * m + if pm then 12 * 3600 else 0)
                  newTime   = if (dayTime' - dayTime) > 0
                      then UTCTime day dayTime'
                      else UTCTime (addDays 1 day) dayTime'
        RRelTime int -> do
            modifyTime newTime
            return (if dayDiff == 0 then [] else [Line newTime DateChange])
            where newTime = addUTCTime (fromIntegral $ int * 60) time
                  dayDiff = diffDays (utctDay newTime) (utctDay time)
        RCommand cmd -> case cmd of
            CJoin nick       -> return [Line time $ Join nick "unknown@unknown"]
            CQuit nick msg   -> return [Line time $ Quit nick "unknown@unknown" msg]
            CNick nick nick' -> return [Line time $ NickChange nick nick']
            CNoNick nick     -> return [Line time $ NoNick nick]
            CCensored        -> return [Line time   Censored]
            CMode ni mde hos -> return [Line time $ Mode ni mde hos]
            CClose           -> return [Line time   LogClose]
            COpen            -> return [Line time   LogOpen ]
            -- TODO: hostnames
        RPMReceive n -> modify (\st -> st { ircSpeaker = n, ircPMState = PMIsReceive } ) *> return []
        RPMSend n    -> modify (\st -> st { ircSpeaker = n, ircPMState = PMIsSend } )    *> return []
        RSnip        -> return [Snip]
        REmpty       -> return []
        RComment _   -> return []
        RRaw str     -> return [Raw str]
    return (ls ++ rawMod)

data Warning = TimeTravelWarning Int deriving (Show)
