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
    Sign(..),
    ASTProcedureDeclaration,
    ASTFormalParameterList,
    ASTFormalParameterSection,
    ASTUnsignedConstant,
    UnsignedConstantDenoter(..),
    ASTUnsignedNumber,
    UnsignedNumberDenoter(..),
    ASTFactor,
    FactorDenoter(..),
    ASTVariableAccess,
    VariableAccessDenoter(..),
    ASTIndexedVariable
    )

import PazLexer (
    ASTIdentifier,
    ASTUnsignedInteger,
    ASTUnsignedReal,
    ASTScaleFactor,
    ASTCharacterString,
    ASTSign,
    Sign(..)
    )

type IndentationLvl = Int
type PprintObj a = (IndentationLvl, a)

-- Helper functions

replace :: PprintObj a -> b -> PprintObj b
replace (lvl, _) x = (lvl, x)

empty :: PprintObj a -> PprintObj ()
empty (lvl, _) = (lvl, ())

levelUp :: PprintObj a -> PprintObj a
levelUp (lvl, x) = (lvl + 1, x)

ast :: PprintObj a -> a
ast (_, x) = x

printSepBy :: IO () -> [IO ()] -> IO ()
printSepBy _ [] = return ()
printSepBy _ [x] = x
printSepBy sep (x:y:zs) =
    x >> sep >> (printSepBy sep (y:zs))

printMaybe :: Maybe a -> (a -> IO ()) -> IO ()
printMaybe Nothing _ = return ()
printMaybe (Just a) f = f a

printIndentation :: IndentationLvl -> IO ()
printIndentation 0 = return ()
printIndentation n = (putStr "    ") >> (printIndentation (n-1))

pprintLineBreak :: PprintObj () -> IO ()
pprintLineBreak (lvl, _) = (putStr "\n") >> (printIndentation lvl)

printSpace :: IO ()
printSpace = putStr " "

-- Program

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


-- Variable declaration part

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
    printMaybe maybeSign (\s -> pprintParserSign $ replace obj s)
    pprintUnsignedInteger $ replace obj unsignedInt

-- have to define two functions for ASTSign in PazParser and PazLexer
pprintParserSign :: PprintObj PazParser.ASTSign -> IO ()
pprintParserSign obj@(_, sign) = case sign of
    PazParser.SignPlus    -> pprintTokenPlus $ empty obj
    PazParser.SignMinus   -> pprintTokenMinus $ empty obj

pprintLexerSign :: PprintObj PazLexer.ASTSign -> IO ()
pprintLexerSign obj@(_, sign) = case sign of
    PazLexer.SignPlus    -> pprintTokenPlus $ empty obj
    PazLexer.SignMinus   -> pprintTokenMinus $ empty obj

pprintUnsignedInteger :: PprintObj ASTUnsignedInteger -> IO ()
pprintUnsignedInteger (_, x) = putStr x

-- Procedure declaration part

pprintProcedureDeclarationPart :: PprintObj ASTProcedureDeclarationPart -> IO ()
pprintProcedureDeclarationPart obj = do
    let decls = ast obj
    let pprintDecl d = (do
        pprintProcedureDeclaration $ replace obj d
        pprintTokenSemicolon $ empty obj)
    printSepBy (pprintLineBreak $ empty obj) (map pprintDecl decls)

pprintProcedureDeclaration :: PprintObj ASTProcedureDeclaration -> IO ()
pprintProcedureDeclaration obj = do
    let e = empty obj
    let (id, maybeParamList, var, com) = ast obj
    pprintTokenProcedure e
    printSpace
    pprintIdentifier $ replace obj id
    printMaybe maybeParamList (\l -> pprintFormalParameterList $ replace obj l)
    pprintTokenSemicolon e
    pprintLineBreak e
    pprintVariableDeclarationPart $ replace obj var
    pprintLineBreak e
    pprintCompondStatement $ replace obj com

pprintFormalParameterList :: PprintObj ASTFormalParameterList -> IO ()
pprintFormalParameterList obj@(_, (p, ps)) = do
    let e = empty obj
    let pList = p:ps
    let printParamSec p = pprintFormalParameterSection $ replace obj p
    pprintTokenLeftParenthesis e
    printSepBy (pprintTokenSemicolon e >> printSpace) (map printParamSec pList)
    pprintTokenRightParenthesis e

pprintFormalParameterSection :: PprintObj ASTFormalParameterSection -> IO ()
pprintFormalParameterSection obj@(_, (bool, idList, typeDenoter)) = do
    let e = empty obj
    case bool of
        True    -> (pprintTokenVar e >> printSpace)
        False   -> return ()
    pprintIdentifierList $ replace obj idList
    pprintTokenColon e
    printSpace
    pprintTypeDenoter $ replace obj typeDenoter

-- Compound statement

pprintCompondStatement :: PprintObj ASTCompoundStatement -> IO ()
pprintCompondStatement obj@(_, factors) = do
    let e = empty obj
    let o2 = levelUp obj
    let e2 = levelUp e
    let pprintFac f = pprintFactor $ replace obj f
    pprintTokenBegin e
    pprintLineBreak e2
    -- placeholder: assume compound statement = [unsigned_constant]
    printSepBy printSpace (map pprintFac factors)
    pprintLineBreak e
    pprintTokenEnd e

pprintFactor :: PprintObj ASTFactor -> IO ()
pprintFactor obj@(_, denoter) =
    case denoter of
        UnsignedConstantDenoter c
            -> pprintUnsignedConstant $ replace obj c
        VariableAccessDenoter v
            -> pprintVariableAccess $ replace obj v
        -- ExpressionDenoter e ->

pprintVariableAccess :: PprintObj ASTVariableAccess -> IO ()
pprintVariableAccess obj@(_, denoter) =
    case denoter of
        IndexedVariableDenoter i
            -> pprintIndexedVariable $ replace obj i
        IdentifierDenoter i
            -> pprintIdentifier $ replace obj i

pprintIndexedVariable :: PprintObj ASTIndexedVariable -> IO ()
pprintIndexedVariable obj@(_, (id, factor)) = do
    pprintIdentifier $ replace obj id
    pprintTokenLeftBracket $ empty obj
    pprintFactor $ replace obj factor
    pprintTokenRightBracket $ empty obj

pprintUnsignedConstant :: PprintObj ASTUnsignedConstant -> IO ()
pprintUnsignedConstant obj@(_, denoter) =
    case denoter of
        UnsignedNumberDenoter n
            -> pprintUnsignedNumber $ replace obj n
        CharacterStringDenoter c
            -> pprintCharacterString $ replace obj c

pprintUnsignedNumber :: PprintObj ASTUnsignedNumber -> IO ()
pprintUnsignedNumber obj@(_, denoter) = 
    case denoter of
        UnsignedIntegerDenoter i
            -> pprintUnsignedInteger $ replace obj i
        UnsignedRealDenoter r
            -> pprintUnsignedReal $ replace obj r

pprintUnsignedReal :: PprintObj ASTUnsignedReal -> IO ()
pprintUnsignedReal obj@(_, (seq, maybeSeq, maybeScale)) = do
    putStr seq
    printMaybe maybeSeq (\s -> do
        pprintTokenDot $ empty obj
        putStr s)
    printMaybe maybeScale (\s -> do
        pprintTokenE $ empty obj
        pprintScaleFactor $ replace obj s)

pprintScaleFactor :: PprintObj ASTScaleFactor -> IO ()
pprintScaleFactor obj@(_, (maybeSign, seq)) = do
    printMaybe maybeSign (\s -> pprintLexerSign $ replace obj s)
    putStr seq

pprintCharacterString :: PprintObj ASTCharacterString -> IO ()
pprintCharacterString obj@(_, s) = do
    pprintTokenSingleQuote $ empty obj
    putStr s
    pprintTokenSingleQuote $ empty obj

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

pprintTokenE :: PprintObj () -> IO ()
pprintTokenE _ = putStr "E"

pprintTokenEqual :: PprintObj () -> IO ()
pprintTokenEqual _ = putStr "="

pprintTokenSingleQuote :: PprintObj () -> IO ()
pprintTokenSingleQuote _ = putStr "'"

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