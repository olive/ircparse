module Main
where

import Text.Printf

import Parser

main :: IO ()
main = do
    putStrLn "hi"
    fileContents <- readFile "text.chat"
    let parseResult = parseToRawLines "text.chat" fileContents
    case parseResult of
        Right lines -> putStr . showLines $ lines
        Left parseError -> print parseError


data Speaker = Speaker { name :: String }

showSpeakerInAngleBrackets :: Speaker -> String
showSpeakerInAngleBrackets s = "< " ++ name s ++ ">"

data Time = Time { year :: Int
                 , month :: Int
                 , day :: Int
                 , hour :: Int
                 , minute :: Int }

showTime :: Time -> String
showTime (Time _ _ _ h m) = printf "%02d:%02d" h m

data IRCState = IRCState { ircSpeaker :: Speaker
                         , ircTime :: Time
                         , ircLines :: [String] }

defaultIrcState :: IRCState
defaultIrcState = IRCState (Speaker "") (Time 2000 1 1 0 0) []


showLines :: [Line] -> String
showLines ls = unlines . reverse . ircLines $ processLines defaultIrcState ls

processLines :: IRCState -> [Line] -> IRCState
processLines = foldl processLine

processLine :: IRCState -> Line -> IRCState
processLine s l = case l of
    LEmpty           -> s
    LComment _       -> s
    LDate m' d' y'   -> case ircTime s of
        Time _ _ _ h m  -> s { ircTime = Time y' m' d' h m }
        -- TODO: day change
    LTime h' m' isPm -> case ircTime s of
        Time y d mo _ _ -> s { ircTime = Time y mo d h' m' }
        -- TODO: isPm
        -- TODO: day change
    LNick str        -> s { ircSpeaker = Speaker str }
        -- TODO: check the nick
    LText str        -> s { ircLines = newLine : ircLines s }
        where newLine = unwords [ showTime (ircTime s)
                                , showSpeakerInAngleBrackets (ircSpeaker s)
                                , str ]
    _                -> s { ircLines = "sorry" : ircLines s }
