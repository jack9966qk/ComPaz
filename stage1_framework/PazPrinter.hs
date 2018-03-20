module PazPrinter where

import PazParser (
    ASTProgram,
    ASTVariableDeclarationPart,
    ASTProcedureDeclarationPart,
    ASTCompoundStatement,
    ASTVariableDeclaration
    )

import PazLexer (
    ASTIdentifier
    )

type IndentationLvl = Int
type PprintObj a = (IndentationLvl, a)

pprintSepBy :: IO () -> [IO ()] -> IO ()
pprintSepBy _ [] = return ()
pprintSepBy _ [x] = x
pprintSepBy sep (x:y:zs) =
    x >> sep >> (pprintSepBy sep (y:zs))

pprintLineBreakWithIndentation :: Int -> IO ()
pprintLineBreakWithIndentation lvl = (putStr "\n") >> (pprintIndentation lvl)

pprintIndentation :: Int -> IO ()
pprintIndentation 0 = return ()
pprintIndentation n = (putStr "    ") >> (pprintIndentation (n-1))

pprintProgram :: ASTProgram -> IO ()
pprintProgram prog = pprintProgram' (0, prog)

pprintProgram' :: PprintObj ASTProgram -> IO ()
pprintProgram' (lvl, (id, var, pro, com)) = do
    pprintTokenProgram (lvl, ())
    putStr " "
    pprintIdentifier (lvl, id)
    pprintSepBy (pprintLineBreakWithIndentation lvl) [
        pprintTokenSemicolon (lvl, ()),
        pprintVariableDeclarationPart (lvl, var),
        pprintProcedureDeclarationPart (lvl, pro),
        pprintCompondStatement (lvl, com)
        ]
    pprintTokenDot (lvl, ())

pprintTokenDot :: PprintObj () -> IO ()
pprintTokenDot _ = putStr "."

pprintTokenSemicolon :: PprintObj () -> IO ()
pprintTokenSemicolon _ = putStr ";"

pprintVariableDeclarationPart :: PprintObj ASTVariableDeclarationPart -> IO ()
pprintVariableDeclarationPart (_, Nothing) = return ()
pprintVariableDeclarationPart (lvl, Just (decl, moreDecl)) = do
    let decls = decl:moreDecl
    let pprintDecls = map (\d -> pprintVariableDeclaration (lvl+1, d)) decls
    let ppLineBreakIndented = pprintLineBreakWithIndentation (lvl+1)
    pprintTokenVar (lvl, ())
    ppLineBreakIndented
    pprintSepBy ppLineBreakIndented pprintDecls

pprintVariableDeclaration :: PprintObj ASTVariableDeclaration -> IO ()
pprintVariableDeclaration _ = putStr "[TODO] Variable Declaration"

pprintProcedureDeclarationPart :: PprintObj ASTProcedureDeclarationPart -> IO ()
pprintProcedureDeclarationPart _ = putStr "[TODO] Procedure Declaration part"

pprintCompondStatement :: PprintObj ASTCompoundStatement -> IO ()
pprintCompondStatement _ = putStr "[TODO] Compound Statement"

pprintIdentifier :: PprintObj ASTIdentifier -> IO ()
pprintIdentifier (_, s) = putStr s


-- Keywords

pprintTokenAnd :: PprintObj () -> IO ()
pprintTokenAnd _ = putStr "and"

pprintTokenArray :: PprintObj () -> IO ()
pprintTokenArray _ = putStr "array"

pprintTokenBegin :: PprintObj () -> IO ()
pprintTokenBegin _ = putStr "begin"

pprintTokenBoolean :: PprintObj () -> IO ()
pprintTokenBoolean _ = putStr "boolean"

pprintTokenDiv :: PprintObj () -> IO ()
pprintTokenDiv _ = putStr "div"

pprintTokenDo :: PprintObj () -> IO ()
pprintTokenDo _ = putStr "do"

pprintTokenDown_to :: PprintObj () -> IO ()
pprintTokenDown_to _ = putStr "down_to"

pprintTokenElse :: PprintObj () -> IO ()
pprintTokenElse _ = putStr "else"

pprintTokenEnd :: PprintObj () -> IO ()
pprintTokenEnd _ = putStr "end"

pprintTokenFor :: PprintObj () -> IO ()
pprintTokenFor _ = putStr "for"

pprintTokenFunction :: PprintObj () -> IO ()
pprintTokenFunction _ = putStr "function"

pprintTokenIf :: PprintObj () -> IO ()
pprintTokenIf _ = putStr "if"

pprintTokenInteger :: PprintObj () -> IO ()
pprintTokenInteger _ = putStr "integer"

pprintTokenNot :: PprintObj () -> IO ()
pprintTokenNot _ = putStr "not"

pprintTokenOf :: PprintObj () -> IO ()
pprintTokenOf _ = putStr "of"

pprintTokenOr :: PprintObj () -> IO ()
pprintTokenOr _ = putStr "or"

pprintTokenProcedure :: PprintObj () -> IO ()
pprintTokenProcedure _ = putStr "procedure"

pprintTokenProgram :: PprintObj () -> IO ()
pprintTokenProgram _ = putStr "program"

pprintTokenReal :: PprintObj () -> IO ()
pprintTokenReal _ = putStr "real"

pprintTokenThen :: PprintObj () -> IO ()
pprintTokenThen _ = putStr "then"

pprintTokenTo :: PprintObj () -> IO ()
pprintTokenTo _ = putStr "to"

pprintTokenVar :: PprintObj () -> IO ()
pprintTokenVar _ = putStr "var"

pprintTokenWhile :: PprintObj () -> IO ()
pprintTokenWhile _ = putStr "while"