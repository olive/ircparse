module Parser
( RawLine(..)
, Command(..)
, parseToRawLines
)
where

import Control.Applicative ((<*>), (<*), (*>), (<$>))
import Data.Char (isSpace)
import Text.Parsec hiding (Line)
import Text.Parsec.String

parseToRawLines :: String -> String -> Either ParseError [(RawLine, Int)]
parseToRawLines = parse pFile

data Command = CJoin String
             | CQuit String String
             | CNick String String
             | CNoNick String
             | CCensored
             | CMode String String String

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
             | RSystem String
             | RText String
             | RTime Int Int Bool    -- Hour Minute IsPm

pFile :: Parser [(RawLine, Int)]
pFile = (pLine `sepBy` endOfLine) <* eof

pLine :: Parser (RawLine, Int)
pLine = (,) <$> (choice [ pDate
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
            <*> (sourceLine <$> getPosition)

pRelTime   =                  RRelTime   <$> pInt
pComment   = char '#'     *> (RComment   <$> pRestOfLine)
pNick      = char '>'     *> (RNick      <$> pManyNoSpace)
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
pPMReceive = string "<-"  *> (RPMReceive <$> pRestOfLine)
pPMSend    = string "->"  *> (RPMSend    <$> pRestOfLine)
pSystem    = char '$'     *> (RSystem    <$> pRestOfLine)
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
