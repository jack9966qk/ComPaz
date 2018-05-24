-- COMP90045 Stage 3, Team ComPaz
-- PazLexerText.hs
-- Test if PazLexer works.

{-# LANGUAGE PackageImports #-}

import Text.Parsec (parse)
import PazLexer (parseStartSymbol)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

main :: IO ()
main = do
    text <- getContents
    case parse parseStartSymbol "(stdin)" text of
        Left error ->
            die (show error)
        Right ast ->
            putStrLn (show ast)
