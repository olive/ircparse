module Parser
( RawLine(..)
, Command(..)
, parseToRawLines
)
where

import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Text.Parsec hiding (Line)
import Text.Parsec.ByteString

parseToRawLines :: String -> ByteString -> Either ParseError [[(RawLine, Int)]]
parseToRawLines = parse pFile

data Command = CJoin String
             | CQuit String String
             | CNick String String
             | CNoNick String
             | CCensored
             | CMode String String String
             | CClose
             | COpen deriving (Show)

data RawLine = RAction String
             | RCommand Command
             | RComment String
             | RDate Int Int Integer -- Month Day Year
             | REmpty
             | RNick String
             | RPMReceive String
             | RPMSend String
             | RRelTime Int
             | RSnip
             | RText String
             | RTime Int Int Bool    -- Hour Minute IsPm
             | RRaw String deriving (Show)

pFile :: Parser [[(RawLine, Int)]]
pFile = many1 (pRawLines <|> pIRCLines) <* eof

pRawLines :: Parser [(RawLine, Int)]
pRawLines = pRawLine `endBy1` endOfLine

pRawLine :: Parser (RawLine, Int)
pRawLine = notFollowedBy (string "%%%") *> ((,) <$> (RRaw <$> pRestOfLine) <*> lineNumber)

pIRCLines :: Parser [(RawLine, Int)]
pIRCLines = pSectionSeperator *> (pIRCLine `endBy1` endOfLine) <* pSectionSeperator

pSectionSeperator :: Parser String
pSectionSeperator = try (string "%%%") <* endOfLine

pIRCLine :: Parser (RawLine, Int)
pIRCLine = (,) <$> (choice [ pDate
                           , try pTime
                             <|> pRelTime
                           , pNick
                           , pCommand
                           , pComment
                           , pPMReceive
                           , try pPMSend
                           , pSystem
                           , pAction
                           , pText
                           , pSnip
                           , pEmpty
                           ] <* many (oneOf " \t"))
               <*> lineNumber

lineNumber :: Parser Int
lineNumber = sourceLine <$> getPosition

pRelTime, pComment, pNick, pCommand, pPMReceive, pPMSend :: Parser RawLine
pSystem,  pAction,  pText, pDate, pTime, pSnip,  pEmpty  :: Parser RawLine
pRelTime   =                  RRelTime   <$> pInt
pComment   = char '#'     *> (RComment   <$> pRestOfLine)
pNick      = char ':'     *> (RNick      <$> pManyNoSpace)
pCommand   = char '/'     *> (RCommand   <$> choice [ CJoin <$> (string "join" *> spaces *> pManyNoSpace) <* pRestOfLine
                                                    , CQuit <$> (string "quit" *> spaces *> pManyNoSpace)
                                                            <*> option "" (char ' ' *> pRestOfLine)
                                                    , CNick <$> (try (string "nick") *> spaces *> pManyNoSpace)
                                                            <*> (spaces *> pManyNoSpace) <* pRestOfLine
                                                    , CNoNick <$> (string "nonick" *> spaces *> pManyNoSpace)
                                                              <* pRestOfLine
                                                    , string "censored" *> return CCensored <* pRestOfLine
                                                    , CMode <$> (string "mode" *> spaces *> pManyNoSpace)
                                                            <*> (spaces *> pManyNoSpace)
                                                            <*> (spaces *> pManyNoSpace) <* pRestOfLine ])
pPMReceive = char '<'     *> (RPMReceive <$> pManyNoSpace)
pPMSend    = char '>'     *> (RPMSend    <$> pManyNoSpace)
pSystem    = char '$'     *> (RCommand   <$> choice [ string "close" *> return CClose
                                                    , string "open"  *> return COpen ])
pAction    = char '*'     *> (RAction    <$> pRestOfLine)
pText      = string "  "  *> (RText      <$> pRestOfLine)
pDate      = char '@'     *> (RDate      <$> (pInt <* char '/')
                                         <*> (pInt <* char '/')
                                         <*> pInt)
pTime      =                  RTime      <$> (pInt <* char ':')
                                         <*> pInt
                                         <*> choice [ string "pm" *> return True
                                                    , string "am" *> return False
                                                    , return False ]
pSnip      = string "---" *> return RSnip
pEmpty     =                 return REmpty

pInt :: (Read a, Integral a) => Parser a
pInt = read <$> many1 digit

pRestOfLine :: Parser String
pRestOfLine = many (noneOf "\n\r")

pManyNoSpace :: Parser String
pManyNoSpace = many (satisfy (not . isSpace))
