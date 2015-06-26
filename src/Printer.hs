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
                                , q ++ "]" ]
        -- TODO: parameterize channel name
    NickChange ol ne -> unwords [ showTimeStamp time
                                , "-!-"
                                , ol
                                , "is now known as"
                                , ne ]
    Censored         -> unwords [ showTimeStamp time
                                , "-!- #bunbun Cannot send to channel (your message contained a censored word)" ]
    PMReceive _ _ -> ""
    PMSend _ _ -> ""
    DateChange       -> formatTime defaultTimeLocale "--- Day changed %a %b %d %Y" time
    LogOpen          -> formatTime defaultTimeLocale "--- Log opened %a %b %d %H:%M:%S %Y" time
    LogClose         -> formatTime defaultTimeLocale "--- Log closed %a %b %d %H:%M:%S %Y" time

showSpeaker :: Nick -> String
showSpeaker n = concat ["< ", n, ">"]

showTimeStamp :: UTCTime -> String
showTimeStamp = formatTime defaultTimeLocale "%H:%M"
