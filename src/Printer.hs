module Printer
where

import Data.Time
import System.Locale (defaultTimeLocale)

import Processor

showLines :: [Line] -> String
showLines = unlines . map showLine

showLine :: Line -> String
showLine Snip              = "\n***SNIP***\n"
showLine (Line time event) = case event of
    Action nick str  -> unwords [ showTimeStamp time
                                , " *"
                                , nick
                                , str ]
    Message nick str -> unwords [ showTimeStamp time
                                , showSpeaker nick
                                , str ]
    Join nick hostna -> unwords [ showTimeStamp time
                                , "-!-"
                                , nick
                                , "[" ++ hostna ++ "]"
                                , "has joined #bunbun" ]
    Quit nick host q -> unwords [ showTimeStamp time
                                , "-!-"
                                , nick
                                , "[" ++ host ++ "]"
                                , "has quit #bunbun"
                                , "[Quit:"
                                , if q /= "" then q else "quit" ++ "]" ]
        -- TODO: parameterize channel name
    NickChange ol ne -> unwords [ showTimeStamp time
                                , "-!-"
                                , ol
                                , "is now known as"
                                , ne ]
    NoNick nick      -> unwords [ showTimeStamp time
                                , "-!-"
                                , nick
                                , ": No such nick/channel" ]
    Censored         -> unwords [ showTimeStamp time
                                , "-!- #bunbun Cannot send to channel (your message contained a censored word)" ]
    PMReceive nick m -> unwords [ showTimeStamp time
                                , "-!-"
                                , nick
                                , "says:"
                                , m ]
    PMSend    nick m -> unwords [ showTimeStamp time
                                , "-!- tell"
                                , nick ++ ":"
                                , m ]
    Mode ni mod hos  -> unwords [ showTimeStamp time
                                , "-!-"
                                , "mode/#bunbun"
                                , "[" ++ mod ++ hos ++ "]"
                                , "by"
                                , ni ]
    DateChange       -> formatTime defaultTimeLocale "--- Day changed %a %b %d %Y" time
    LogOpen          -> formatTime defaultTimeLocale "--- Log opened %a %b %d %H:%M:%S %Y" time
    LogClose         -> formatTime defaultTimeLocale "--- Log closed %a %b %d %H:%M:%S %Y" time

showSpeaker :: Nick -> String
showSpeaker n = concat ["< ", n, ">"]

showTimeStamp :: UTCTime -> String
showTimeStamp = formatTime defaultTimeLocale "%H:%M"

showWarnings :: [Warning] -> String
showWarnings = unlines . map showWarning

showWarning :: Warning -> String
showWarning (TimeTravelWarning line) = "WARNING: time travel detected around line " ++ show line
