module Main
where

import Parser
import Processor
import Printer

import Data.ByteString hiding (unzip, map, concatMap, length, putStr, hPutStr, putStrLn, head)
import System.Environment (getArgs)
import System.IO (utf8, IOMode(..), Handle, openFile, hSetEncoding, hPutStr, withFile) 

getUtf8Handle :: FilePath -> IOMode -> IO Handle
getUtf8Handle fp iom = do
    file <- openFile fp iom
    hSetEncoding file utf8
    return file

readUtf8 :: FilePath -> IO ByteString
readUtf8 fp = do
    file <- getUtf8Handle fp ReadMode
    hGetContents file

getRawLines :: FilePath -> IO [[(RawLine, Int)]]
getRawLines fp = do
    fileContents <- readUtf8 fp
    case parseToRawLines fp fileContents of
        Left  parseError -> print parseError >> return [[]]
        Right rawLines   -> return rawLines

writeLinesToFile :: FilePath -> [[(RawLine, Int)]] -> IO ()
writeLinesToFile fp rawLines = do
    -- handle <- getUtf8Handle fp WriteMode
    let (ls, warnings) = unzip . map processRawLines $ rawLines
    let outputString = concatMap showLines ls
    withFile fp WriteMode (\handle -> do
        hSetEncoding handle utf8
        hPutStr handle outputString )
    mapM_ (putStr . showWarnings) warnings

main :: IO ()
main = do
    args <- getArgs
    if length args < 2
        then putStrLn "not enough arguments given"
        else writeLinesToFile (args !! 1) =<< getRawLines (head args)
