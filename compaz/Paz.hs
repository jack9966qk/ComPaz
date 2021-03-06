-- COMP90045 Stage 3, Team ComPaz
-- Paz.hs
-- Driver code that lexes, parses and then generates code for Paz source file.

{-# LANGUAGE PackageImports #-}

import Debug.Trace (trace)
import Text.Parsec (parse)
import PazLexer
import PazParser
import PazPrinter
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Codegen

die :: String -> IO ()
die err = do
    hPutStrLn stderr err
    exitFailure

fullParse :: String -> IO (Either String PazParser.ASTProgram)
fullParse text = do
    case
        trace
            "*** Lexical analysis"
            (parse PazLexer.parseStartSymbol "(stdin)" text)
        of
        Left error -> do
            die ("Lexical error:\n" ++ show error)
            return (Left "Lexical Error")
        Right tokens ->
            case
                trace
                    "*** Syntax analysis"
                    (parse PazParser.parseStartSymbol "(stdin)" tokens)
                of
                Left error -> do
                    die ("Syntax error:\n" ++ show error)
                    return (Left "Syntax Error")
                Right ast ->
                    return (Right ast)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-p", filename] ->
            (do
                text <- readFile filename
                parseResult <- fullParse text
                case parseResult of
                    Left _ -> return ()
                    Right ast ->
                        trace 
                            "AST result:\n\n======================"
                            (pprintProgram ast)
            )
        [filename] ->
            (do
                text <- readFile filename
                parseResult <- fullParse text
                case parseResult of
                    Left _ -> return ()
                    Right ast ->
                        trace 
                            "IR Code:\n\n======================"
                            (generateCode ast)
            )
        _   -> putStrLn "Usage: Paz [-p] source_file"
