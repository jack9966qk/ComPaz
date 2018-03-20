module PazPrinter where

import PazParser (
    ASTProgram,
    ASTVariableDeclarationPart,
    ASTProcedureDeclarationPart,
    ASTCompoundStatement
    )

import PazLexer (
    ASTIdentifier
    )


pprintProgram :: ASTProgram -> IO ()
pprintProgram (id, var, pro, com) = do
    pprintTokenProgram
    putStr " "
    pprintIdentifier id
    pprintTokenSemicolon
    putStr "\n\n"
    pprintVariableDeclarationPart var
    putStr "\n\n"
    pprintProcedureDeclarationPart pro
    putStr "\n\n"
    pprintCompondStatement com
    pprintTokenDot

pprintTokenDot :: IO ()
pprintTokenDot = putStr "."

pprintTokenProgram :: IO ()
pprintTokenProgram = putStr "program"

pprintTokenSemicolon :: IO ()
pprintTokenSemicolon = putStr ";"

pprintVariableDeclarationPart :: ASTVariableDeclarationPart -> IO ()
pprintVariableDeclarationPart _ = putStr "[TODO] Variable Declaration Part"

pprintProcedureDeclarationPart :: ASTProcedureDeclarationPart -> IO ()
pprintProcedureDeclarationPart _ = putStr "[TODO] Procedure Declaration part"

pprintCompondStatement :: ASTCompoundStatement -> IO ()
pprintCompondStatement _ = putStr "[TODO] Compound Statement"

pprintIdentifier :: ASTIdentifier -> IO ()
pprintIdentifier = putStr


-- astPrettyShow :: ASTProgram -> String
-- astPrettyShow (id, var, pro, com) =
--     "ID: " ++ (show id) ++ "\n" ++
--     "Var: " ++ (show var) ++ "\n" ++
--     "Pro: " ++ (show pro) ++ "\n" ++
--     "Com: " ++ (show com)