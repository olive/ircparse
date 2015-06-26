module Parser
( Line(..)
, parseToRawLines
)
where

import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Data.Char (isSpace)
import Text.Parsec hiding (Line)
import Text.Parsec.String

parseToRawLines :: String -> String -> Either ParseError [Line]
parseToRawLines = parse pFile

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
