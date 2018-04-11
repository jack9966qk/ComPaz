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
    ASTIndexedVariable,
    ASTTerm,
    ASTMultiplyingOperator,
    MultiplyingOperatorDenoter(..),
    ASTSimpleExpression,
    ASTAddingOperator,
    AddingOperatorDenoter(..),
    ASTExpression,
    ASTRelationalOperator,
    RelationalOperatorDenoter(..),
    ASTAssignmentStatement,
    ASTLvalue,
    LvalueDenoter(..),
    ASTStatement,
    ASTStatementDenoter(..),
    ASTProcedureStatement,
    ASTActualParameterList,
    ASTIfStatement,
    ASTWhileStatement,
    ASTForStatement,
    ToDownToDenoter(..)
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

import PazUtil(
    Context(..),
    exprChildContext,
    simpExprChildContext,
    termChildContext,
    needParen
    )

type IndentationLvl = Int
type PprintObj a = (PazUtil.Context, IndentationLvl, a)

-- Helper functions

replace :: PprintObj a -> b -> PprintObj b
replace (c, lvl, _) x = (c, lvl, x)

empty :: PprintObj a -> PprintObj ()
empty (c, lvl, _) = (c, lvl, ())

levelUp :: PprintObj a -> PprintObj a
levelUp (c, lvl, x) = (c, lvl + 1, x)

-- accessor function
ast :: PprintObj a -> a
ast (_, _, x) = x

-- accessor function
context :: PprintObj a -> Context
context (c, _, _) = c

setContext :: PprintObj a -> Context -> PprintObj a
setContext (_, lvl, x) c = (c, lvl, x)

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
pprintLineBreak (_, lvl, _) = (putStr "\n") >> (printIndentation lvl)

printSpace :: IO ()
printSpace = putStr " "

isCompound :: ASTStatement -> Bool
isCompound (CompoundStatementDenoter _)  = True
isCompound _                             = False

levelUpIfNotCompound :: PprintObj a -> ASTStatement -> PprintObj a
levelUpIfNotCompound obj stmt =
    if isCompound stmt then obj else levelUp obj

-- Program

pprintProgram :: ASTProgram -> IO ()
pprintProgram prog = pprintProgram' (Atomic, 0, prog)

pprintProgram' :: PprintObj ASTProgram -> IO ()
pprintProgram' obj = do
    let e = empty obj
    let (id, var, pro, com) = ast obj
    let lineBreak = pprintLineBreak e
    -- program identification part
    pprintTokenProgram e
    printSpace
    pprintIdentifier $ replace obj id
    pprintTokenSemicolon e
    -- variable declaration part, add line break if not empty
    case var of
        Nothing -> return ()
        _       -> lineBreak >> lineBreak
    pprintVariableDeclarationPart $ replace obj var
    -- procedure declaration part, add line break if not empty
    case pro of
        []  -> return ()
        _   -> lineBreak >> lineBreak
    pprintProcedureDeclarationPart $ replace obj pro
    -- compound statement (program's body)
    lineBreak >> lineBreak
    pprintCompondStatement $ replace obj com
    pprintTokenDot e


-- Variable declaration part

pprintVariableDeclarationPart :: PprintObj ASTVariableDeclarationPart -> IO ()
pprintVariableDeclarationPart obj =
    case ast obj of
        Nothing -> return ()
        Just (decl, moreDecl) -> (do
            let e = empty obj
            let e2 = levelUp e
            let decls = decl:moreDecl
            let pprintDecl d = do
                pprintVariableDeclaration $ replace e2 d
                pprintTokenSemicolon e
            pprintTokenVar e
            pprintLineBreak e2
            printSepBy (pprintLineBreak e2) (map pprintDecl decls)
            )

pprintVariableDeclaration :: PprintObj ASTVariableDeclaration -> IO ()
pprintVariableDeclaration obj = do
    let (idList, typeDenoter) = ast obj
    pprintIdentifierList $ replace obj idList
    pprintTokenColon $ empty obj
    printSpace
    pprintTypeDenoter $ replace obj typeDenoter
    
pprintIdentifierList :: PprintObj ASTIdentifierList -> IO ()
pprintIdentifierList obj = do
    let (id, moreId) = ast obj
    let idList = id:moreId
    let pprintId x = pprintIdentifier $ replace obj x
    printSepBy (pprintTokenComma $ empty obj) (map pprintId idList)

pprintTypeDenoter :: PprintObj ASTTypeDenoter -> IO ()
pprintTypeDenoter obj =
    case ast obj of
        OrdinaryTypeDenoter id ->
            pprintTypeIdentifier $ replace obj id
        ArrayTypeDenoter arrayType ->
            pprintArrayType $ replace obj arrayType

pprintTypeIdentifier :: PprintObj ASTTypeIdentifier -> IO ()
pprintTypeIdentifier obj =
    let e = empty obj in
        case ast obj of
            IntegerTypeIdentifier   -> pprintTokenInteger e
            RealTypeIdentifier      -> pprintTokenReal e
            BooleanTypeIdentifier   -> pprintTokenBoolean e

pprintArrayType :: PprintObj ASTArrayType -> IO ()
pprintArrayType obj = do
    let e = empty obj
    let (subrangeType, typeId) = ast obj
    pprintTokenArray e
    pprintTokenLeftBracket e
    pprintSubrangeType $ replace obj subrangeType
    pprintTokenRightBracket e
    printSpace
    pprintTokenOf e
    printSpace
    pprintTypeIdentifier $ replace obj typeId

pprintSubrangeType :: PprintObj ASTSubrangeType -> IO ()
pprintSubrangeType obj = do
    let (c1, c2) = ast obj
    pprintConstant $ replace obj c1
    pprintTokenEllipsis $ empty obj
    pprintConstant $ replace obj c2

pprintConstant :: PprintObj ASTConstant -> IO ()
pprintConstant obj = do
    let (maybeSign, unsignedInt) = ast obj
    printMaybe maybeSign (\s -> pprintParserSign $ replace obj s)
    pprintUnsignedInteger $ replace obj unsignedInt

-- have to define two functions for ASTSign in PazParser and PazLexer
pprintParserSign :: PprintObj PazParser.ASTSign -> IO ()
pprintParserSign obj = case ast obj of
    PazParser.SignPlus    -> pprintTokenPlus $ empty obj
    PazParser.SignMinus   -> pprintTokenMinus $ empty obj

pprintLexerSign :: PprintObj PazLexer.ASTSign -> IO ()
pprintLexerSign obj = case ast obj of
    PazLexer.SignPlus    -> pprintTokenPlus $ empty obj
    PazLexer.SignMinus   -> pprintTokenMinus $ empty obj

pprintUnsignedInteger :: PprintObj ASTUnsignedInteger -> IO ()
pprintUnsignedInteger obj = putStr $ ast obj

-- Procedure declaration part

pprintProcedureDeclarationPart :: PprintObj ASTProcedureDeclarationPart -> IO ()
pprintProcedureDeclarationPart obj = do
    let decls = ast obj
    let pprintDecl d = (do
        pprintProcedureDeclaration $ replace obj d
        pprintTokenSemicolon $ empty obj)
    let printSep = do
        pprintLineBreak $ empty obj
        pprintLineBreak $ empty obj
    printSepBy printSep (map pprintDecl decls)

pprintProcedureDeclaration :: PprintObj ASTProcedureDeclaration -> IO ()
pprintProcedureDeclaration obj = do
    let e = empty obj
    let (id, maybeParamList, var, com) = ast obj
    pprintTokenProcedure e
    printSpace
    pprintIdentifier $ replace obj id
    printMaybe maybeParamList (\l -> pprintFormalParameterList $ replace obj l)
    pprintTokenSemicolon e
    -- line break only if var declaration part not empty
    case var of
        Nothing -> return ()
        _       -> pprintLineBreak e
    pprintVariableDeclarationPart $ replace obj var
    pprintLineBreak e
    pprintCompondStatement $ replace obj com

pprintFormalParameterList :: PprintObj ASTFormalParameterList -> IO ()
pprintFormalParameterList obj = do
    let (p, ps) = ast obj
    let e = empty obj
    let pList = p:ps
    let printParamSec p = pprintFormalParameterSection $ replace obj p
    pprintTokenLeftParenthesis e
    printSepBy (pprintTokenSemicolon e >> printSpace) (map printParamSec pList)
    pprintTokenRightParenthesis e

pprintFormalParameterSection :: PprintObj ASTFormalParameterSection -> IO ()
pprintFormalParameterSection obj = do
    let (bool, idList, typeDenoter) = ast obj
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
pprintCompondStatement obj = do
    let terms = ast obj
    let e = empty obj
    let o2 = levelUp obj
    let e2 = levelUp e
    let pStatement s = pprintStatement $ replace e2 s
    let printSep = (do
        pprintTokenSemicolon e2
        pprintLineBreak e2)
    pprintTokenBegin e
    pprintLineBreak e2
    printSepBy printSep (map pStatement terms)
    pprintLineBreak e
    pprintTokenEnd e

pprintStatement :: PprintObj ASTStatement -> IO ()
pprintStatement obj =
    case ast obj of
        AssignmentStatementDenoter s
            -> pprintAssignmentStatement $ replace obj s
        ProcedureStatementDenoter s
            -> pprintProcedureStatement $ replace obj s
        CompoundStatementDenoter s
            -> pprintCompondStatement $ replace obj s
        IfStatementDenoter s
            -> pprintIfStatement $ replace obj s
        WhileStatementDenoter s
            -> pprintWhileStatement $ replace obj s
        ForStatementDenoter s
            -> pprintForStatement $ replace obj s
        EmptyStatementDenoter
            -> pprintEmptyStatement $ empty obj

pprintEmptyStatement :: PprintObj () -> IO ()
pprintEmptyStatement _ = return ()

pprintIfStatement :: PprintObj ASTIfStatement -> IO ()
pprintIfStatement obj = do
    let (expr, stmt, maybeElse) = ast obj
    let e = empty obj
    let e2 = levelUpIfNotCompound e stmt
    pprintTokenIf e
    printSpace
    pprintExpression $ replace obj expr
    printSpace
    pprintTokenThen e
    -- statement after "then"
    pprintLineBreak e2
    pprintStatement $ replace e2 stmt
    -- optional "else" part
    printMaybe maybeElse (\s -> do
        let e2' = levelUpIfNotCompound e s
        pprintLineBreak e
        pprintTokenElse e
        pprintLineBreak e2'
        pprintStatement $ replace e2' s
        )

pprintWhileStatement :: PprintObj ASTWhileStatement -> IO ()
pprintWhileStatement obj = do
    let (expr, stmt) = ast obj
    let e = empty obj
    let e2 = levelUpIfNotCompound e stmt
    pprintTokenWhile e
    printSpace
    pprintExpression $ replace obj expr
    printSpace
    pprintTokenDo e
    pprintLineBreak e2
    pprintStatement $ replace e2 stmt

pprintForStatement :: PprintObj ASTForStatement -> IO ()
pprintForStatement obj = do
    let (id, expr, toDownto, bodyExpr, stmt) = ast obj
    let e = empty obj
    let e2 = levelUpIfNotCompound e stmt
    pprintTokenFor e
    printSpace
    pprintIdentifier $ replace obj id
    printSpace
    pprintTokenAssign e
    printSpace
    pprintExpression $ replace obj expr
    printSpace
    case toDownto of
        ToDenoter -> pprintTokenTo e
        DownToDenoter -> pprintTokenDownTo e
    printSpace
    pprintExpression $ replace obj bodyExpr
    printSpace
    pprintTokenDo $ empty obj
    pprintLineBreak e2
    pprintStatement $ replace e2 stmt


pprintProcedureStatement :: PprintObj ASTProcedureStatement -> IO ()
pprintProcedureStatement obj = do
    let (id, maybeParamList) = ast obj
    pprintIdentifier $ replace obj id
    printMaybe maybeParamList (\l ->
        pprintActualParameterList $ replace obj l)

pprintActualParameterList :: PprintObj ASTActualParameterList -> IO ()
pprintActualParameterList obj = do
    let expressions = ast obj
    let pprintExpr e = pprintExpression $ replace obj e
    let printSep = (do
        pprintTokenComma $ empty obj
        printSpace)
    pprintTokenLeftParenthesis $ empty obj
    printSepBy printSep (map pprintExpr expressions)
    pprintTokenRightParenthesis $ empty obj
    
pprintAssignmentStatement :: PprintObj ASTAssignmentStatement -> IO ()
pprintAssignmentStatement obj = do
    let (lval, expr) = ast obj
    case lval of
        LvalueVariableAccessDenoter var
            -> pprintVariableAccess $ replace obj var
        LvalueIdentifierDenoter id
            -> pprintIdentifier $ replace obj id
    printSpace
    pprintTokenAssign $ empty obj
    printSpace
    pprintExpression $ replace obj expr

pprintExpression :: PprintObj ASTExpression -> IO ()
pprintExpression obj = do
    let (simpleExp, maybeModifier) = ast obj
    let o2 = setContext obj (exprChildContext (ast obj) (context obj))
    pprintSimpleExpression $ replace o2 simpleExp
    printMaybe maybeModifier (\(op, simpleExp) -> do
        printSpace
        pprintRelationalOperator $ replace o2 op
        printSpace
        pprintSimpleExpression $ replace o2 simpleExp
        )

pprintRelationalOperator :: PprintObj ASTRelationalOperator -> IO ()
pprintRelationalOperator obj =
    case ast obj of
        EqualDenoter
            -> pprintTokenEqual $ empty obj
        NotEqualDenoter
            -> pprintTokenNotEqual $ empty obj
        LessThanDenoter
            -> pprintTokenLessThan $ empty obj
        GreaterThanDenoter
            -> pprintTokenGreaterThan $ empty obj
        LessThanOrEqualDenoter
            -> pprintTokenLessThanOrEqual $ empty obj
        GreaterThanOrEqualDenoter
            -> pprintTokenGreaterThanOrEqual $ empty obj

pprintSimpleExpression :: PprintObj ASTSimpleExpression -> IO ()
pprintSimpleExpression obj = do
    let (maybeSign, term, modifiers) = ast obj
    let o2 = setContext obj (simpExprChildContext (ast obj) (context obj))
    let printModifier (op, t) = (do
        printSpace
        pprintAddingOperator $ replace o2 op
        printSpace
        pprintTerm $ replace o2 t)
    printMaybe maybeSign (\s -> pprintParserSign $ replace o2 s)
    pprintTerm $ replace o2 term
    printSepBy (return ()) (map printModifier modifiers)

pprintAddingOperator :: PprintObj ASTAddingOperator -> IO ()
pprintAddingOperator obj =
    case ast obj of
        PlusDenoter    -> pprintTokenPlus $ empty obj
        MinusDenoter   -> pprintTokenMinus $ empty obj
        OrDenoter      -> pprintTokenOr $ empty obj

-- The passed expression may be part of a factor that uses parentheses. But
-- don't print parentheses unless they are needed.
pprintFactorExpr :: PprintObj ASTExpression -> Bool -> IO ()
pprintFactorExpr obj False = pprintExpression obj
pprintFactorExpr obj True = (do
    pprintTokenLeftParenthesis $ empty obj
    pprintExpression obj
    pprintTokenRightParenthesis $ empty obj)

pprintFactor :: PprintObj ASTFactor -> IO ()
pprintFactor obj = do
    case ast obj of
        UnsignedConstantDenoter c
            -> pprintUnsignedConstant $ replace obj c
        VariableAccessDenoter v
            -> pprintVariableAccess $ replace obj v
        ExpressionDenoter e
            -> pprintFactorExpr (replace obj e) (needParen e (context obj))
        NegatedFactorDenoter f
            -> (do
                pprintTokenNot $ empty obj
                printSpace
                pprintFactor $ replace obj f)

pprintTerm :: PprintObj ASTTerm -> IO ()
pprintTerm obj = do
    let (factor, modifiers) = ast obj
    let o2 = setContext obj (termChildContext (ast obj) (context obj))
    let pprintModifier (mult, fac) = (do
        printSpace
        pprintMultiplyingOperator $ replace o2 mult
        printSpace
        pprintFactor $ replace o2 fac)
    pprintFactor $ replace o2 factor
    printSepBy (return ()) (map pprintModifier modifiers)

pprintMultiplyingOperator :: PprintObj ASTMultiplyingOperator -> IO ()
pprintMultiplyingOperator obj =
    case ast obj of
        TimesDenoter    -> pprintTokenTimes $ empty obj
        DivideByDenoter -> pprintTokenDivideBy $ empty obj
        DivDenoter      -> pprintTokenDiv $ empty obj
        AndDenoter      -> pprintTokenAnd $ empty obj

pprintVariableAccess :: PprintObj ASTVariableAccess -> IO ()
pprintVariableAccess obj =
    case ast obj of
        IndexedVariableDenoter i
            -> pprintIndexedVariable $ replace obj i
        IdentifierDenoter i
            -> pprintIdentifier $ replace obj i

pprintIndexedVariable :: PprintObj ASTIndexedVariable -> IO ()
pprintIndexedVariable obj = do
    let (id, expression) = ast obj
    pprintIdentifier $ replace obj id
    pprintTokenLeftBracket $ empty obj
    pprintExpression $ replace obj expression
    pprintTokenRightBracket $ empty obj

pprintUnsignedConstant :: PprintObj ASTUnsignedConstant -> IO ()
pprintUnsignedConstant obj =
    case ast obj of
        UnsignedNumberDenoter n
            -> pprintUnsignedNumber $ replace obj n
        CharacterStringDenoter c
            -> pprintCharacterString $ replace obj c

pprintUnsignedNumber :: PprintObj ASTUnsignedNumber -> IO ()
pprintUnsignedNumber obj = 
    case ast obj of
        UnsignedIntegerDenoter i
            -> pprintUnsignedInteger $ replace obj i
        UnsignedRealDenoter r
            -> pprintUnsignedReal $ replace obj r

pprintUnsignedReal :: PprintObj ASTUnsignedReal -> IO ()
pprintUnsignedReal obj = do
    let (seq, maybeSeq, maybeScale) = ast obj
    putStr seq
    printMaybe maybeSeq (\s -> do
        pprintTokenDot $ empty obj
        putStr s)
    printMaybe maybeScale (\s -> do
        pprintTokenE $ empty obj
        pprintScaleFactor $ replace obj s)

pprintScaleFactor :: PprintObj ASTScaleFactor -> IO ()
pprintScaleFactor obj = do
    let (maybeSign, seq) = ast obj
    printMaybe maybeSign (\s -> pprintLexerSign $ replace obj s)
    putStr seq

pprintCharacterString :: PprintObj ASTCharacterString -> IO ()
pprintCharacterString obj = do
    let s = ast obj
    pprintTokenSingleQuote $ empty obj
    putStr s
    pprintTokenSingleQuote $ empty obj

-- Tokens

pprintIdentifier :: PprintObj ASTIdentifier -> IO ()
pprintIdentifier obj = putStr $ ast obj

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

pprintTokenDownTo :: PprintObj () -> IO ()
pprintTokenDownTo _ = putStr "downto"

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
