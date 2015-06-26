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
    DateChange       -> formatTime defaultTimeLocale "--- Day changed %a %b %d %Y" time
    _                -> ""

showSpeaker :: Nick -> String
showSpeaker n = concat ["< ", n, ">"]

showTimeStamp :: UTCTime -> String
showTimeStamp = formatTime defaultTimeLocale "%H:%M"
