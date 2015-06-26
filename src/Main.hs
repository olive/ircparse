module Main
where

import Parser
import Processor
import Printer

main :: IO ()
main = do
    fileContents <- readFile "text.chat"
    let parseResult = parseToRawLines "text.chat" fileContents
    case parseResult of
        Right rawLines   -> do
            let (lines, warnings) = processRawLines rawLines
            putStr . showLines $ lines
            -- TODO: print warnings
        Left  parseError -> print parseError
