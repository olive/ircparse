module Main
where

import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Data.Char (isSpace)
import Text.Parsec hiding (Line)
import Text.Parsec.String
import Text.Printf

main :: IO ()
main = do
    putStrLn "hi"
    fileContents <- readFile "text.chat"
    let parseResult = parse pFile "text.chat" fileContents
    case parseResult of
        Right lines -> putStr . showLines $ lines
        Left parseError -> print parseError

data Line = LText String
          | LAction String
          | LDate Int Int Int -- Month Day Year
          | LTime Int Int Bool    -- Hour Minute IsPm
          | LNick String
          | LRelTime Int
          | LComment String
          | LSystem String
          | LSnip
          | LPMSend String
          | LPMReceive String
          | LCommand String 
          | LEmpty deriving (Show)

pFile :: Parser [Line]
pFile = (pLine `sepBy` endOfLine) <* eof

pLine :: Parser Line
pLine = choice [ pDate
               , try pTime <|> pRelTime
               , pNick
               , pCommand
               , pComment
               , pPMReceive
               , try pPMSend
               , pSystem
               , pAction
               , pText
               , string "---" *> return LSnip
               , return LEmpty
               ] <* many (oneOf " \t")

pDate :: Parser Line
pDate = char '@' *> (LDate <$> intWithSlash <*> intWithSlash <*> pInt)
    where intWithSlash = pInt <* char '/'

pTime :: Parser Line
pTime = LTime <$> (pInt <* char ':')
              <*> pInt
              <*> ((string "pm" *> return True) <|> (string "am" *> return False) <|> return False)

pRelTime :: Parser Line
pRelTime = LRelTime <$> pInt

pComment :: Parser Line
pComment = char '#' *> (LComment <$> pRestOfLine)

pNick :: Parser Line
pNick = char '>' *> (LNick <$> many (satisfy (not . isSpace)))

pCommand :: Parser Line
pCommand = char '/' *> (LCommand <$> pRestOfLine)

pPMReceive :: Parser Line
pPMReceive = string "<-" *> (LPMReceive <$> pRestOfLine)

pPMSend :: Parser Line
pPMSend = string "->" *> (LPMSend <$> pRestOfLine)

pSystem :: Parser Line
pSystem = char '$' *> (LSystem <$> pRestOfLine)

pAction :: Parser Line
pAction = char '*' *> (LAction <$> pRestOfLine)

pText :: Parser Line
pText = string "  " *> (LText <$> pRestOfLine)

pInt :: Parser Int
pInt = read <$> many1 digit

pRestOfLine :: Parser String
pRestOfLine = many (noneOf "\n\r")


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
