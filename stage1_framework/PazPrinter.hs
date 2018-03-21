module PazPrinter where

import PazParser (
    ASTProgram,
    ASTVariableDeclarationPart,
    ASTProcedureDeclarationPart,
    ASTCompoundStatement,
    ASTVariableDeclaration,
    ASTIdentifierList,
    ASTTypeDenoter,
    TypeDenoter(..),
    ASTTypeIdentifier,
    TypeIdentifier(..),
    ASTArrayType,
    ASTSubrangeType,
    ASTConstant,
    ASTSign,
    Sign(..)
    )

import PazLexer (
    ASTIdentifier,
    ASTUnsignedInteger
    )

type IndentationLvl = Int
type PprintObj a = (IndentationLvl, a)

printSepBy :: IO () -> [IO ()] -> IO ()
printSepBy _ [] = return ()
printSepBy _ [x] = x
printSepBy sep (x:y:zs) =
    x >> sep >> (printSepBy sep (y:zs))

printIndentation :: Int -> IO ()
printIndentation 0 = return ()
printIndentation n = (putStr "    ") >> (printIndentation (n-1))

pprintLineBreak :: PprintObj () -> IO ()
pprintLineBreak (lvl, _) = (putStr "\n") >> (printIndentation lvl)

replace :: PprintObj a -> b -> PprintObj b
replace (lvl, _) x = (lvl, x)

empty :: PprintObj a -> PprintObj ()
empty (lvl, _) = (lvl, ())

levelUp :: PprintObj a -> PprintObj a
levelUp (lvl, x) = (lvl + 1, x)

ast :: PprintObj a -> a
ast (_, x) = x

printSpace :: IO ()
printSpace = putStr " "

pprintProgram :: ASTProgram -> IO ()
pprintProgram prog = pprintProgram' (0, prog)

pprintProgram' :: PprintObj ASTProgram -> IO ()
pprintProgram' obj@(lvl, (id, var, pro, com)) = do
    let e = empty obj
    let lineBreak = pprintLineBreak e
    pprintTokenProgram e
    printSpace
    pprintIdentifier $ replace obj id
    printSepBy (lineBreak >> lineBreak) [
        pprintTokenSemicolon e,
        pprintVariableDeclarationPart $ replace obj var,
        pprintProcedureDeclarationPart $ replace obj pro,
        pprintCompondStatement $ replace obj com
        ]
    pprintTokenDot e



pprintVariableDeclarationPart :: PprintObj ASTVariableDeclarationPart -> IO ()
pprintVariableDeclarationPart (_, Nothing) = return ()
pprintVariableDeclarationPart obj@(lvl, Just (decl, moreDecl)) = do
    let e = empty obj
    let e2 = levelUp e
    let decls = decl:moreDecl
    let pprintDecl d = do
        pprintVariableDeclaration $ replace e2 d
        pprintTokenSemicolon e
    pprintTokenVar e
    pprintLineBreak e2
    printSepBy (pprintLineBreak e2) (map pprintDecl decls)

pprintVariableDeclaration :: PprintObj ASTVariableDeclaration -> IO ()
pprintVariableDeclaration obj = do
    let (idList, typeDenoter) = ast obj
    pprintIdentifierList $ replace obj idList
    pprintTokenColon $ empty obj
    printSpace
    pprintTypeDenoter $ replace obj typeDenoter
    
pprintIdentifierList :: PprintObj ASTIdentifierList -> IO ()
pprintIdentifierList obj@(_, (id, moreId)) = do
    let idList = id:moreId
    let pprintId x = pprintIdentifier $ replace obj x
    printSepBy (pprintTokenComma $ empty obj) (map pprintId idList)

pprintTypeDenoter :: PprintObj ASTTypeDenoter -> IO ()
pprintTypeDenoter obj@(_, OrdinaryTypeDenoter id) =
    pprintTypeIdentifier $ replace obj id

pprintTypeDenoter obj@(_, ArrayTypeDenoter arrayType) = do
    pprintArrayType $ replace obj arrayType

pprintTypeIdentifier :: PprintObj ASTTypeIdentifier -> IO ()
pprintTypeIdentifier obj =
    let e = empty obj in
        case ast obj of
            IntegerTypeIdentifier   -> pprintTokenInteger e
            RealTypeIdentifier      -> pprintTokenReal e
            BooleanTypeIdentifier   -> pprintTokenBoolean e

pprintArrayType :: PprintObj ASTArrayType -> IO ()
pprintArrayType obj@(_, (subrangeType, typeId)) = do
    let e = empty obj
    pprintTokenArray e
    printSpace
    pprintTokenLeftBracket e
    pprintSubrangeType $ replace obj subrangeType
    pprintTokenRightBracket e
    printSpace
    pprintTokenOf e
    printSpace
    pprintTypeIdentifier $ replace obj typeId

pprintSubrangeType :: PprintObj ASTSubrangeType -> IO ()
pprintSubrangeType obj@(_, (c1, c2)) = do
    pprintConstant $ replace obj c1
    pprintTokenEllipsis $ empty obj
    pprintConstant $ replace obj c2

pprintConstant :: PprintObj ASTConstant -> IO ()
pprintConstant obj@(_, (maybeSign, unsignedInt)) = do
    case maybeSign of
        Just sign -> pprintSign $ replace obj sign
        Nothing   -> return ()
    pprintUnsignedInteger $ replace obj unsignedInt

pprintSign :: PprintObj ASTSign -> IO ()
pprintSign obj@(_, sign) = case sign of
    SignPlus    -> pprintTokenPlus $ empty obj
    SignMinus   -> pprintTokenMinus $ empty obj

pprintUnsignedInteger :: PprintObj ASTUnsignedInteger -> IO ()
pprintUnsignedInteger (_, x) = putStr x

pprintProcedureDeclarationPart :: PprintObj ASTProcedureDeclarationPart -> IO ()
pprintProcedureDeclarationPart _ = putStr "[TODO] Procedure Declaration part"

pprintCompondStatement :: PprintObj ASTCompoundStatement -> IO ()
pprintCompondStatement _ = putStr "[TODO] Compound Statement"

-- Tokens

pprintIdentifier :: PprintObj ASTIdentifier -> IO ()
pprintIdentifier (_, s) = putStr s

pprintTokenLeftParenthesis :: PprintObj () -> IO ()
pprintTokenLeftParenthesis _ = putStr "("

pprintTokenRightParenthesis :: PprintObj () -> IO ()
pprintTokenRightParenthesis _ = putStr ")"

pprintTokenTimes :: PprintObj () -> IO ()
pprintTokenTimes _ = putStr "*"

pprintTokenPlus :: PprintObj () -> IO ()
pprintTokenPlus _ = putStr "+"

pprintTokenComma :: PprintObj () -> IO ()
pprintTokenComma _ = putStr ","

pprintTokenMinus :: PprintObj () -> IO ()
pprintTokenMinus _ = putStr "-"

pprintTokenEllipsis :: PprintObj () -> IO ()
pprintTokenEllipsis _ = putStr ".."

pprintTokenDot :: PprintObj () -> IO ()
pprintTokenDot _ = putStr "."

pprintTokenDivideBy :: PprintObj () -> IO ()
pprintTokenDivideBy _ = putStr "/"

pprintTokenAssign :: PprintObj () -> IO ()
pprintTokenAssign _ = putStr ":="

pprintTokenColon :: PprintObj () -> IO ()
pprintTokenColon _ = putStr ":"

pprintTokenSemicolon :: PprintObj () -> IO ()
pprintTokenSemicolon _ = putStr ";"

pprintTokenLessThanOrEqual :: PprintObj () -> IO ()
pprintTokenLessThanOrEqual _ = putStr "<="

pprintTokenNotEqual :: PprintObj () -> IO ()
pprintTokenNotEqual _ = putStr "<>"

pprintTokenLessThan :: PprintObj () -> IO ()
pprintTokenLessThan _ = putStr "<"

pprintTokenEqual :: PprintObj () -> IO ()
pprintTokenEqual _ = putStr "="

pprintTokenGreaterThanOrEqual :: PprintObj () -> IO ()
pprintTokenGreaterThanOrEqual _ = putStr ">="

pprintTokenGreaterThan :: PprintObj () -> IO ()
pprintTokenGreaterThan _ = putStr ">"

pprintTokenLeftBracket :: PprintObj () -> IO ()
pprintTokenLeftBracket _ = putStr "["

pprintTokenRightBracket :: PprintObj () -> IO ()
pprintTokenRightBracket _ = putStr "]"

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