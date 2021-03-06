-- COMP90045 Stage 3, Team ComPaz
-- PazParser.hs
-- Generate an AST for a Paz source file serving as input to Codegen.

module PazParser where

import Debug.Trace (trace)
import Text.Parsec (
    Parsec,
    SourcePos,
    choice,
    eof,
    optionMaybe,
    optional,
    parse,
    tokenPrim,
    try
    )
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (void)
import Control.Applicative (many) -- get <|> from here too if needed
import PazLexer (
    ASTLexicalToken,
    LexicalToken(..),
    ASTCharacterString,
    ASTIdentifier,
    ASTUnsignedInteger,
    ASTUnsignedReal
    )

-- define a parser which parses an incoming stream of ASTLexicalToken,
-- noting that ASTLexicalToken is a pair of SourcePos and LexicalToken
type Parser = Parsec [ASTLexicalToken] ()

-- since we are not processing characters we must define "satisfy" ourself,
-- tokenPrim takes a show function, an update function, and test function
satisfy ::
     -- the following type constraint is some kind of Haskell gobbledygook,
     -- to make it work we'd have to turn on the FlexibleContents feature:
     --(Stream s m ASTLexicalToken) =>
     (LexicalToken -> Bool) -> Parser LexicalToken
satisfy f =
     tokenPrim
         (\(_, x) -> show x)
         updatePosASTLexicalToken
         (\(_, x) -> if f x then Just x else Nothing)

-- updating the source position is simply extracing it from the coming
-- ASTLexicalToken in the input stream, if there is no next parseToken then
-- use current (i.e. last) parseToken, I am sure there would be a better way
updatePosASTLexicalToken ::
    SourcePos -> ASTLexicalToken -> [ASTLexicalToken] -> SourcePos
updatePosASTLexicalToken _ _ ((pos, _) : _) = pos
updatePosASTLexicalToken _ (pos, _) [] = pos

parseTokenEof :: Parser ()
parseTokenEof = eof

-- lexical tokens section
-- these are basically wrappers that check for each kind of lexical parseToken
-- already recognized by PazLexer.hs and then regurgitate it, those
-- starting with "parseToken" throw away the ASTLexicalToken, those starting
-- with "parse" unwrap the ASTLexicalToken and then the LexicalToken

parseTokenLeftParenthesis :: Parser ()
parseTokenLeftParenthesis =
    void (
        satisfy (
            \x ->
                case x of
                    LTLeftParenthesis -> True
                    otherwise -> False
            )
        )
    
parseTokenRightParenthesis :: Parser ()
parseTokenRightParenthesis =
    void (
        satisfy (
            \x ->
                case x of
                    LTRightParenthesis -> True
                    otherwise -> False
            )
        )
    
parseTokenTimes :: Parser ()
parseTokenTimes =
    void (
        satisfy (
            \x ->
                case x of
                    LTTimes -> True
                    otherwise -> False
            )
        )
    
parseTokenPlus :: Parser ()
parseTokenPlus =
    void (
        satisfy (
            \x ->
                case x of
                    LTPlus -> True
                    otherwise -> False
            )
        )
    
parseTokenComma :: Parser ()
parseTokenComma =
    void (
        satisfy (
            \x ->
                case x of
                    LTComma -> True
                    otherwise -> False
            )
        )
    
parseTokenMinus :: Parser ()
parseTokenMinus =
    void (
        satisfy (
            \x ->
                case x of
                    LTMinus -> True
                    otherwise -> False
            )
        )
    
parseTokenEllipsis :: Parser ()
parseTokenEllipsis =
    void (
        satisfy (
            \x ->
                case x of
                    LTEllipsis -> True
                    otherwise -> False
            )
        )
    
parseTokenDot :: Parser ()
parseTokenDot =
    void (
        satisfy (
            \x ->
                case x of
                    LTDot -> True
                    otherwise -> False
            )
        )
    
parseTokenDivideBy :: Parser ()
parseTokenDivideBy =
    void (
        satisfy (
            \x ->
                case x of
                    LTDivideBy -> True
                    otherwise -> False
            )
        )
    
parseTokenAssign :: Parser ()
parseTokenAssign =
    void (
        satisfy (
            \x ->
                case x of
                    LTAssign -> True
                    otherwise -> False
            )
        )
    
parseTokenColon :: Parser ()
parseTokenColon =
    void (
        satisfy (
            \x ->
                case x of
                    LTColon -> True
                    otherwise -> False
            )
        )
    
parseTokenSemicolon :: Parser ()
parseTokenSemicolon =
    void (
        satisfy (
            \x ->
                case x of
                    LTSemicolon -> True
                    otherwise -> False
            )
        )
    
parseTokenLessThanOrEqual :: Parser ()
parseTokenLessThanOrEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTLessThanOrEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenNotEqual :: Parser ()
parseTokenNotEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTNotEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenLessThan :: Parser ()
parseTokenLessThan =
    void (
        satisfy (
            \x ->
                case x of
                    LTLessThan -> True
                    otherwise -> False
            )
        )
    
parseTokenEqual :: Parser ()
parseTokenEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenGreaterThanOrEqual :: Parser ()
parseTokenGreaterThanOrEqual =
    void (
        satisfy (
            \x ->
                case x of
                    LTGreaterThanOrEqual -> True
                    otherwise -> False
            )
        )
    
parseTokenGreaterThan :: Parser ()
parseTokenGreaterThan =
    void (
        satisfy (
            \x ->
                case x of
                    LTGreaterThan -> True
                    otherwise -> False
            )
        )
    
parseTokenLeftBracket :: Parser ()
parseTokenLeftBracket =
    void (
        satisfy (
            \x ->
                case x of
                    LTLeftBracket -> True
                    otherwise -> False
            )
        )
    
parseTokenRightBracket :: Parser ()
parseTokenRightBracket =
    void (
        satisfy (
            \x ->
                case x of
                    LTRightBracket -> True
                    otherwise -> False
            )
        )
    
parseTokenAnd :: Parser ()
parseTokenAnd =
    void (
        satisfy (
            \x ->
                case x of
                    LTAnd -> True
                    otherwise -> False
            )
        )

parseTokenArray :: Parser ()
parseTokenArray =
    void (
        satisfy (
            \x ->
                case x of
                    LTArray -> True
                    otherwise -> False
            )
        )

parseTokenBegin :: Parser ()
parseTokenBegin =
    void (
        satisfy (
            \x ->
                case x of
                    LTBegin -> True
                    otherwise -> False
            )
        )

parseTokenBoolean :: Parser ()
parseTokenBoolean =
    void (
        satisfy (
            \x ->
                case x of
                    LTBoolean -> True
                    otherwise -> False
            )
        )

parseTokenDiv :: Parser ()
parseTokenDiv =
    void (
        satisfy (
            \x ->
                case x of
                    LTDiv -> True
                    otherwise -> False
            )
        )

parseTokenDo :: Parser ()
parseTokenDo =
    void (
        satisfy (
            \x ->
                case x of
                    LTDo -> True
                    otherwise -> False
            )
        )

parseTokenDownTo :: Parser ()
parseTokenDownTo =
    void (
        satisfy (
            \x ->
                case x of
                    LTDownTo -> True
                    otherwise -> False
            )
        )

parseTokenElse :: Parser ()
parseTokenElse =
    void (
        satisfy (
            \x ->
                case x of
                    LTElse -> True
                    otherwise -> False
            )
        )

parseTokenEnd :: Parser ()
parseTokenEnd =
    void (
        satisfy (
            \x ->
                case x of
                    LTEnd -> True
                    otherwise -> False
            )
        )

parseTokenFor :: Parser ()
parseTokenFor =
    void (
        satisfy (
            \x ->
                case x of
                    LTFor -> True
                    otherwise -> False
            )
        )

parseTokenFunction :: Parser ()
parseTokenFunction =
    void (
        satisfy (
            \x ->
                case x of
                    LTFunction -> True
                    otherwise -> False
            )
        )

parseTokenIf :: Parser ()
parseTokenIf =
    void (
        satisfy (
            \x ->
                case x of
                    LTIf -> True
                    otherwise -> False
            )
        )

parseTokenInteger :: Parser ()
parseTokenInteger =
    void (
        satisfy (
            \x ->
                case x of
                    LTInteger -> True
                    otherwise -> False
            )
        )

parseTokenNot :: Parser ()
parseTokenNot =
    void (
        satisfy (
            \x ->
                case x of
                    LTNot -> True
                    otherwise -> False
            )
        )

parseTokenOf :: Parser ()
parseTokenOf =
    void (
        satisfy (
            \x ->
                case x of
                    LTOf -> True
                    otherwise -> False
            )
        )

parseTokenOr :: Parser ()
parseTokenOr =
    void (
        satisfy (
            \x ->
                case x of
                    LTOr -> True
                    otherwise -> False
            )
        )

parseTokenProcedure :: Parser ()
parseTokenProcedure =
    void (
        satisfy (
            \x ->
                case x of
                    LTProcedure -> True
                    otherwise -> False
            )
        )

parseTokenProgram :: Parser ()
parseTokenProgram =
    void (
        satisfy (
            \x ->
                case x of
                    LTProgram -> True
                    otherwise -> False
            )
        )

parseTokenReal :: Parser ()
parseTokenReal =
    void (
        satisfy (
            \x ->
                case x of
                    LTReal -> True
                    otherwise -> False
            )
        )

parseTokenThen :: Parser ()
parseTokenThen =
    void (
        satisfy (
            \x ->
                case x of
                    LTThen -> True
                    otherwise -> False
            )
        )

parseTokenTo :: Parser ()
parseTokenTo =
    void (
        satisfy (
            \x ->
                case x of
                    LTTo -> True
                    otherwise -> False
            )
        )

parseTokenVar :: Parser ()
parseTokenVar =
    void (
        satisfy (
            \x ->
                case x of
                    LTVar -> True
                    otherwise -> False
            )
        )

parseTokenWhile :: Parser ()
parseTokenWhile =
    void (
        satisfy (
            \x ->
                case x of
                    LTWhile -> True
                    otherwise -> False
            )
        )

parseTokenRead :: Parser ()
parseTokenRead =
    void (
        satisfy (
            \x ->
                case x of
                    LTRead -> True
                    otherwise -> False
            )
        )

parseTokenWrite :: Parser ()
parseTokenWrite =
    void (
        satisfy (
            \x ->
                case x of
                    LTWrite -> True
                    otherwise -> False
            )
        )

parseTokenWriteln :: Parser ()
parseTokenWriteln =
    void (
        satisfy (
            \x ->
                case x of
                    LTWriteln -> True
                    otherwise -> False
            )
        )

parseTokenFalse :: Parser ()
parseTokenFalse =
    void (
        satisfy (
            \x ->
                case x of
                    LTFalse -> True
                    otherwise -> False
            )
        )

parseTokenTrue :: Parser ()
parseTokenTrue =
    void (
        satisfy (
            \x ->
                case x of
                    LTTrue -> True
                    otherwise -> False
            )
        )

parseCharacterString :: Parser ASTCharacterString
parseCharacterString =
    do
        (LTCharacterString x) <-
            satisfy (
                \x ->
                    case x of
                        LTCharacterString _ -> True
                        otherwise -> False
                )
        return x

parseIdentifier :: Parser ASTIdentifier
parseIdentifier =
    do
        (LTIdentifier x) <-
            satisfy (
                \x ->
                    case x of
                        LTIdentifier _ -> True
                        otherwise -> False
                )
        return x

parseUnsignedInteger :: Parser ASTUnsignedInteger
parseUnsignedInteger =
    do
        (LTUnsignedInteger x) <-
            satisfy (
                \x ->
                    case x of
                        LTUnsignedInteger _ -> True
                        otherwise -> False
                )
        return x

parseUnsignedReal :: Parser ASTUnsignedReal
parseUnsignedReal =
    do
        (LTUnsignedReal x) <-
            satisfy (
                \x ->
                    case x of
                        LTUnsignedReal _ -> True
                        otherwise -> False
                )
        return x

-- end of lexical tokens section
 
type ASTStartSymbol = ASTProgram
parseStartSymbol :: Parser ASTStartSymbol
parseStartSymbol =
    trace
        "parseStartSymbol"
        (
            do
                x0 <-
                    parseProgram
                parseTokenEof
                return x0
            )

type ASTProgram = (ASTIdentifier, ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTCompoundStatement)
-- type ASTProgram = (ASTIdentifier, ASTVariableDeclarationPart, ASTProcedureDeclarationPart, ASTUnsignedConstant)
parseProgram :: Parser ASTProgram
parseProgram =
    trace
        "parseProgram"
        (
            do
                parseTokenProgram
                x0 <-
                    parseIdentifier
                parseTokenSemicolon
                x1 <-
                    parseVariableDeclarationPart
                x2 <-
                    parseProcedureDeclarationPart
                x3 <-
                    parseCompoundStatement
                parseTokenDot
                return (x0, x1, x2, x3)
            )

type ASTProcedureDeclarationPart = [ASTProcedureDeclaration]
-- type ASTProcedureDeclarationPart = ()
parseProcedureDeclarationPart :: Parser ASTProcedureDeclarationPart
parseProcedureDeclarationPart =
    trace
        "parseProcedureDeclarationPart"
        (
            many (
                try (
                    do
                        x0 <-
                            parseProcedureDeclaration
                        parseTokenSemicolon
                        return x0
                    )
                )
            )

-- your code starts here
type ASTBooleanConstant = BooleanConstantDenoter
data BooleanConstantDenoter =
    FalseDenoter |
    TrueDenoter
    deriving(Show)
parseBooleanConstant :: Parser ASTBooleanConstant
parseBooleanConstant =
    trace
        "parseBooleanConstant"
        (
            choice
                [
                    try (
                        do
                            parseTokenFalse
                            return FalseDenoter
                        ),
                    do
                        parseTokenTrue
                        return TrueDenoter
                ]
        )

type ASTUnsignedConstant = UnsignedConstantDenoter
data UnsignedConstantDenoter =
    BooleanConstantDenoter ASTBooleanConstant |
    UnsignedIntegerDenoter ASTUnsignedInteger |
    UnsignedRealDenoter ASTUnsignedReal
    deriving(Show)
parseUnsignedConstantDenoter :: Parser ASTUnsignedConstant
parseUnsignedConstantDenoter =
    trace
        "parseUnsignedConstantDenoter"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseBooleanConstant
                            return (BooleanConstantDenoter x)
                        ),
                    try (
                        do
                            x <-
                                parseUnsignedInteger
                            return (UnsignedIntegerDenoter x)
                        ),
                    do
                        x <-
                            parseUnsignedReal
                        return (UnsignedRealDenoter x)
                ]
        )

type ASTFactor = FactorDenoter
data FactorDenoter =
    UnsignedConstantDenoter ASTUnsignedConstant |
    VariableAccessDenoter ASTVariableAccess |
    ExpressionDenoter ASTExpression |
    NegatedFactorDenoter ASTFactor
    deriving(Show)
parseFactorDenoter :: Parser ASTFactor
parseFactorDenoter =
    trace
        "parseFactorDenoter"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseUnsignedConstantDenoter
                            return (UnsignedConstantDenoter x)
                        ),
                    try (
                        do
                            x <-
                                parseVariableAccessDenoter
                            return (VariableAccessDenoter x)
                        ),
                    try (
                        do
                            parseTokenLeftParenthesis
                            x <-
                                parseExpression
                            parseTokenRightParenthesis
                            return (ExpressionDenoter x)
                        ),
                    do
                        parseTokenNot
                        x <-
                            parseFactorDenoter
                        return (NegatedFactorDenoter x)
                ]
        )

type ASTIndexedVariable = (ASTIdentifier, ASTExpression)
parseIndexedVariable :: Parser ASTIndexedVariable
parseIndexedVariable =
    trace
        "parseIndexedVariable"
        (
            try (
                do
                    x0 <-
                        parseIdentifier
                    parseTokenLeftBracket
                    x1 <-
                        parseExpression
                    parseTokenRightBracket
                    return (x0, x1)
                )
        )

type ASTVariableAccess = VariableAccessDenoter
data VariableAccessDenoter =
    IndexedVariableDenoter ASTIndexedVariable |
    IdentifierDenoter ASTIdentifier
    deriving(Show)
parseVariableAccessDenoter :: Parser VariableAccessDenoter
parseVariableAccessDenoter =
    trace
        "parseVariableAccessDenoter"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseIndexedVariable
                            return (IndexedVariableDenoter x)
                        ),
                    do
                        x <-
                            parseIdentifier
                        return (IdentifierDenoter x)
                ]
        )

type ASTMultiplyingOperator = MultiplyingOperatorDenoter
data MultiplyingOperatorDenoter =
    TimesDenoter |
    DivideByDenoter |
    DivDenoter |
    AndDenoter 
    deriving(Show)
parseMultiplyingOperatorDenoter :: Parser MultiplyingOperatorDenoter
parseMultiplyingOperatorDenoter  =
    trace
        "parseMultiplyingOperatorDenoter"
        (
            choice
                [
                    try (
                        do
                            parseTokenTimes 
                            return TimesDenoter
                        ),
                    try (
                        do
                            parseTokenDivideBy
                            return DivideByDenoter
                        ),
                    try (
                        do
                            parseTokenDiv
                            return DivDenoter
                        ),
                    do
                        parseTokenAnd
                        return AndDenoter
                ]
        )
 
type ASTPostFactorModifier = (MultiplyingOperatorDenoter, FactorDenoter)
parsePostFactorModifier :: Parser ASTPostFactorModifier 
parsePostFactorModifier =
    trace
        "parsePostFactorModifier"
            try (
                do
                    x0 <-
                        parseMultiplyingOperatorDenoter
                    x1 <-
                        parseFactorDenoter
                    return (x0, x1)
            )

type ASTTerm = (ASTFactor, [ASTPostFactorModifier])
parseTerm :: Parser ASTTerm
parseTerm =
    trace
        "parseTerm"
            try (
                do
                    x0 <-
                        parseFactorDenoter
                    x1 <-
                        many (
                            try (
                                parsePostFactorModifier
                                )
                            )
                    return (x0, x1)
            )

type ASTAddingOperator = AddingOperatorDenoter
data AddingOperatorDenoter =
    PlusDenoter |
    MinusDenoter |
    OrDenoter
    deriving(Show)
parseAddingOperatorDenoter :: Parser AddingOperatorDenoter
parseAddingOperatorDenoter  =
    trace
        "parseAddingOperatorDenoter"
        (
            choice
                [
                    try (
                        do
                            parseTokenPlus
                            return PlusDenoter
                        ),
                    try (
                        do
                            parseTokenMinus
                            return MinusDenoter
                        ),
                    do
                        parseTokenOr
                        return OrDenoter
                ]
        )
 
type ASTPostTermModifier = (AddingOperatorDenoter, ASTTerm)
parsePostTermModifier :: Parser ASTPostTermModifier 
parsePostTermModifier =
    trace
        "parseTermModifier"
            try (
                do
                    x0 <-
                        parseAddingOperatorDenoter
                    x1 <-
                        parseTerm
                    return (x0, x1)
            )

type ASTSimpleExpression = ((Maybe ASTSign), ASTTerm, [ASTPostTermModifier])
parseSimpleExpression :: Parser ASTSimpleExpression  
parseSimpleExpression =
    trace
        "parseSimpleExpression"
            try (
                do
                    x0 <-
                        optionMaybe (
                            try (
                                parseSign
                                )
                            )
                    x1 <-
                        parseTerm
                    x2 <-
                        many (
                            try (
                                parsePostTermModifier
                                )
                            )
                    return (x0, x1, x2)
            )

type ASTRelationalOperator = RelationalOperatorDenoter
data RelationalOperatorDenoter =
    EqualDenoter |
    NotEqualDenoter |
    LessThanDenoter |
    GreaterThanDenoter |
    LessThanOrEqualDenoter |
    GreaterThanOrEqualDenoter
    deriving(Show)
parseRelationalOperatorDenoter :: Parser RelationalOperatorDenoter
parseRelationalOperatorDenoter  =
    trace
        "parseRelationalOperatorDenoter"
        (
            choice
                [
                    try (
                        do
                            parseTokenEqual
                            return EqualDenoter
                        ),
                    try (
                        do
                            parseTokenNotEqual
                            return NotEqualDenoter
                        ),
                    try (
                        do
                            parseTokenLessThan
                            return LessThanDenoter
                        ),
                    try (
                        do
                            parseTokenGreaterThan
                            return GreaterThanDenoter
                        ),
                    try (
                        do
                            parseTokenLessThanOrEqual
                            return LessThanOrEqualDenoter
                        ),
                    do
                        parseTokenGreaterThanOrEqual
                        return GreaterThanOrEqualDenoter
                ]
        )

type ASTPostSimpleExpressionModifier = (RelationalOperatorDenoter, ASTSimpleExpression)
parsePostSimpleExpressionModifier :: Parser ASTPostSimpleExpressionModifier 
parsePostSimpleExpressionModifier =
    trace
        "parseSimpleExpressionModifier"
            try (
                do
                    x0 <-
                        parseRelationalOperatorDenoter
                    x1 <-
                        parseSimpleExpression
                    return (x0, x1)
            )

type ASTExpression = (ASTSimpleExpression, (Maybe ASTPostSimpleExpressionModifier))
parseExpression :: Parser ASTExpression  
parseExpression =
    trace
        "parseExpression"
            try (
                do
                    x0 <-
                        parseSimpleExpression
                    x1 <-
                        optionMaybe (
                            try (
                                parsePostSimpleExpressionModifier
                                )
                            )
                    return (x0, x1)
            )

type ASTAssignmentStatement = (VariableAccessDenoter, ASTExpression)
parseAssignmentStatement  :: Parser ASTAssignmentStatement
parseAssignmentStatement =
    trace
        "parseAssignmentStatement"
            try (
                do
                    x0 <-
                        parseVariableAccessDenoter
                    parseTokenAssign
                    x1 <-
                        parseExpression
                    return (x0, x1)
            )

type ASTReadStatement = ASTVariableAccess
parseReadStatement  :: Parser ASTReadStatement
parseReadStatement =
    trace
        "parseReadStatement"
            try (
                do
                    parseTokenRead
                    parseTokenLeftParenthesis
                    x0 <-
                        parseVariableAccessDenoter
                    parseTokenRightParenthesis
                    return x0
            )

type ASTWriteStatement = ASTExpression
parseWriteStatement :: Parser ASTWriteStatement
parseWriteStatement =
    trace
        "parseWriteStatement"
            try (
                do
                    parseTokenWrite
                    parseTokenLeftParenthesis
                    x0 <-
                        parseExpression
                    parseTokenRightParenthesis
                    return x0
            )

type ASTWriteStringStatement = ASTCharacterString
parseWriteStringStatement :: Parser ASTWriteStringStatement
parseWriteStringStatement =
    trace
        "parseWriteStringStatement"
            try (
                do
                    parseTokenWrite
                    parseTokenLeftParenthesis
                    x0 <-
                        parseCharacterString
                    parseTokenRightParenthesis
                    return x0
            )

type ASTWritelnStatement = ()
parseWritelnStatement :: Parser ASTWritelnStatement
parseWritelnStatement =
    trace
        "parseWritelnStatement"
            try (
                do
                    parseTokenWriteln
                    return ()
            )

parseNonPrimaryParameter :: Parser ASTExpression
parseNonPrimaryParameter =
    trace
        "parseNonPrimaryParamater"
            try (
                do
                    parseTokenComma
                    x0 <-
                        parseExpression
                    return x0
            )

type ASTActualParameterList = [ASTExpression]
parseActualParameterList :: Parser ASTActualParameterList
parseActualParameterList =
    trace
        "parseActualParameterList"
            try (
                do
                    parseTokenLeftParenthesis
                    x0 <-
                        parseExpression
                    x1 <-
                        many (
                            try (
                                parseNonPrimaryParameter
                            )
                        )
                    parseTokenRightParenthesis
                    return (x0:x1)
            )

type ASTProcedureStatement = (ASTIdentifier, (Maybe ASTActualParameterList))
parseProcedureStatement :: Parser ASTProcedureStatement
parseProcedureStatement =
    trace
        "parseProcedureStatement"
            try (
                do
                    x0 <-
                        parseIdentifier
                    x1 <-
                        optionMaybe (
                            try (
                                parseActualParameterList
                                )
                            )
                    return (x0, x1)
                )

type ASTStatement = ASTStatementDenoter
data ASTStatementDenoter =
    AssignmentStatementDenoter ASTAssignmentStatement |
    ReadStatementDenoter ASTReadStatement |
    WriteStatementDenoter ASTWriteStatement |
    WriteStringStatementDenoter ASTWriteStringStatement |
    WritelnStatementDenoter ASTWritelnStatement |
    ProcedureStatementDenoter ASTProcedureStatement |
    CompoundStatementDenoter ASTCompoundStatement |
    IfStatementDenoter ASTIfStatement |
    WhileStatementDenoter ASTWhileStatement |
    ForStatementDenoter ASTForStatement |
    EmptyStatementDenoter
    deriving(Show)
parseStatement :: Parser ASTStatement
parseStatement =
    trace
        "parseStatement"
            choice
                [
                    try (
                        do
                            x0 <-
                                parseAssignmentStatement
                            return (AssignmentStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseReadStatement
                            return (ReadStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseWriteStatement
                            return (WriteStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseWriteStringStatement
                            return (WriteStringStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseWritelnStatement
                            return (WritelnStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseProcedureStatement
                            return (ProcedureStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseCompoundStatement
                            return (CompoundStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseIfStatement
                            return (IfStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseWhileStatement
                            return (WhileStatementDenoter x0)
                        ),
                    try (
                        do
                            x0 <-
                                parseForStatement
                            return (ForStatementDenoter x0)
                        ),
                    do
                        parseEmptyStatement
                        return EmptyStatementDenoter
                ]

type ASTNonPrimaryStatement = ASTStatement
parseNonPrimaryStatement :: Parser ASTNonPrimaryStatement  
parseNonPrimaryStatement =
    trace
        "parseNonPrimaryStatement"
            try (
                do
                    parseTokenSemicolon
                    x0 <-
                        parseStatement
                    return x0
            )

type ASTStatementSequence = [ASTStatement]
parseStatementSequence :: Parser ASTStatementSequence 
parseStatementSequence =
    trace
        "parseStatementSequence"
            try (
                do
                    x0 <-
                        parseStatement
                    x1 <-
                        many (
                            try (
                                parseNonPrimaryStatement
                                )
                            )
                    return (x0:x1)
            )

type ASTCompoundStatement = [ASTStatement]
parseCompoundStatement :: Parser ASTCompoundStatement
parseCompoundStatement =
    trace
        "parseCompoundStatement"
        (
            try (
                do
                    -- parseSkipLexicalToken
                    parseTokenBegin
                    x0 <-
                        parseStatementSequence
                    parseTokenEnd
                    return x0
                )
        )

type ASTElseClause = ASTStatement
parseElseClause :: Parser ASTElseClause
parseElseClause =
    trace
        "parseElseClause"
        (
            try (
                do
                    parseTokenElse
                    x0 <-
                        parseStatement
                    return x0
                )
        )

type ASTIfStatement = (ASTExpression, ASTStatement, (Maybe ASTStatement))
parseIfStatement :: Parser ASTIfStatement
parseIfStatement =
    trace
        "parseIfStatement"
        (
            try (
                do
                    parseTokenIf
                    x0 <-
                        parseExpression
                    parseTokenThen
                    x1 <-
                        parseStatement
                    return x0
                    x2 <-
                        optionMaybe (
                            try (
                                parseElseClause
                                )
                            )
                    return (x0, x1, x2)
                )
        )

type ASTWhileStatement = (ASTExpression, ASTStatement)
parseWhileStatement :: Parser ASTWhileStatement
parseWhileStatement =
    trace
        "parseWhileStatement"
        (
            try (
                do
                    parseTokenWhile
                    x0 <-
                        parseExpression
                    parseTokenDo
                    x1 <-
                        parseStatement
                    return (x0, x1)
                )
        )

type ASTToDownTo = ToDownToDenoter
data ToDownToDenoter =
    ToDenoter |
    DownToDenoter
    deriving(Show)
parseToDownTo :: Parser ASTToDownTo
parseToDownTo =
    trace
        "parseToDownTo"
        (
            choice
                [
                    try (
                        do
                            parseTokenTo
                            return ToDenoter
                        ),
                    do
                        parseTokenDownTo
                        return DownToDenoter
                ]
        )

type ASTForStatement = (ASTIdentifier, ASTExpression, ASTToDownTo, ASTExpression, ASTStatement)
parseForStatement :: Parser ASTForStatement
parseForStatement =
    trace
        "parseForStatement"
        (
            try (
                do
                    parseTokenFor
                    x0 <-
                        parseIdentifier
                    parseTokenAssign
                    x1 <-
                        parseExpression
                    x2 <-
                        parseToDownTo
                    x3 <-
                        parseExpression
                    parseTokenDo
                    x4 <-
                        parseStatement
                    return (x0, x1, x2, x3, x4)
                )
        )

type ASTEmptyStatement = ()
parseEmptyStatement :: Parser ASTEmptyStatement
parseEmptyStatement =
    trace
        "parseEmptyStatement"
        (
            return ()
        )

-- type ASTSkipLexicalToken = ()
-- parseSkipLexicalToken :: Parser ASTSkipLexicalToken
-- parseSkipLexicalToken =
    -- trace
        -- "parseSkipLexicalToken"
        -- (
            -- choice
                -- [
                    -- try (
                        -- parseCompoundStatement
                        -- ),
                    -- void (
                        -- satisfy (
                            -- \x ->
                                -- case x of
                                    -- LTEnd -> False
                                    -- _ -> True
                            -- )
                        -- )
                    -- ]
            -- )

-- your code ends here

type ASTProcedureDeclaration = (ASTIdentifier, (Maybe ASTFormalParameterList), ASTVariableDeclarationPart, ASTCompoundStatement)
parseProcedureDeclaration :: Parser ASTProcedureDeclaration
parseProcedureDeclaration =
    trace
        "parseProcedureDeclaration"
        (
            do
                parseTokenProcedure
                x0 <-
                    parseIdentifier
                x1 <-
                    optionMaybe (
                        try (
                            parseFormalParameterList
                            )
                        )
                parseTokenSemicolon
                x2 <-
                    parseVariableDeclarationPart
                x3 <-
                    parseCompoundStatement
                return (x0, x1, x2, x3)
            )

type ASTFormalParameterList = (ASTFormalParameterSection, [ASTFormalParameterSection])
parseFormalParameterList :: Parser ASTFormalParameterList
parseFormalParameterList =
    trace
        "parseFormalParameterList"
        (
            do
                parseTokenLeftParenthesis
                x0 <-
                    parseFormalParameterSection
                x1 <-
                    many (
                        try (
                            do
                                parseTokenSemicolon
                                x0 <-
                                    parseFormalParameterSection
                                return x0
                            )
                        )
                parseTokenRightParenthesis
                return (x0, x1)
            )

-- presence of "var" keyword should have type "Maybe ()" according to the
-- usual system, but the generator cannot handle this because it is stupid,
-- so we manually put in a type of "Bool" which is "True" if "var" present
type ASTFormalParameterSection = (Bool, ASTIdentifierList, ASTTypeDenoter)
parseFormalParameterSection :: Parser ASTFormalParameterSection
parseFormalParameterSection =
    trace
        "parseFormalParameterSection"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseTokenVar
                            )
                        )
                x1 <-
                    parseIdentifierList
                parseTokenColon
                x2 <-
                    parseTypeDenoter
                return (
                    case x0 of
                        Just _ -> (True, x1, x2)
                        _ -> (False, x1, x2)
                    )
            )

type ASTIdentifierList = (ASTIdentifier, [ASTIdentifier])
parseIdentifierList :: Parser ASTIdentifierList
parseIdentifierList =
    trace
        "parseIdentifierList"
        (
            do
                x0 <-
                    parseIdentifier
                x1 <-
                    many (
                        try (
                            do
                                parseTokenComma
                                x0 <-
                                    parseIdentifier
                                return x0
                            )
                        )
                return (x0, x1)
            )

type ASTVariableDeclarationPart = (Maybe (ASTVariableDeclaration, [ASTVariableDeclaration]))
parseVariableDeclarationPart :: Parser ASTVariableDeclarationPart
parseVariableDeclarationPart =
    trace
        "parseVariableDeclarationPart"
        (
            optionMaybe (
                try (
                    do
                        parseTokenVar
                        x0 <-
                            parseVariableDeclaration
                        parseTokenSemicolon
                        x1 <-
                            many (
                                try (
                                    do
                                        x0 <-
                                            parseVariableDeclaration
                                        parseTokenSemicolon
                                        return x0
                                    )
                                )
                        return (x0, x1)
                    )
                )
            )

type ASTVariableDeclaration = (ASTIdentifierList, ASTTypeDenoter)
parseVariableDeclaration :: Parser ASTVariableDeclaration
parseVariableDeclaration =
    trace
        "parseVariableDeclaration"
        (
            do
                x0 <-
                    parseIdentifierList
                parseTokenColon
                x1 <-
                    parseTypeDenoter
                return (x0, x1)
            )

type ASTTypeDenoter = TypeDenoter
data TypeDenoter =
    OrdinaryTypeDenoter ASTTypeIdentifier |
    ArrayTypeDenoter ASTArrayType
    deriving(Eq)

instance Show TypeDenoter where
    show (OrdinaryTypeDenoter typ) = show typ
    show (ArrayTypeDenoter (_, typ)) =
        "array[" ++ (show typ) ++ "]"


parseTypeDenoter :: Parser ASTTypeDenoter
parseTypeDenoter =
    trace
        "parseTypeDenoter"
        (
            choice
                [
                    try (
                        do
                            x <-
                                parseTypeIdentifier
                            return (OrdinaryTypeDenoter x)
                        ),
                    do
                        x <-
                            parseArrayType
                        return (ArrayTypeDenoter x)
                    ]
            )

type ASTTypeIdentifier = TypeIdentifier
data TypeIdentifier =
    IntegerTypeIdentifier |
    RealTypeIdentifier |
    BooleanTypeIdentifier
    deriving(Eq)

instance Show TypeIdentifier where
    show IntegerTypeIdentifier = "int"
    show RealTypeIdentifier = "real"
    show BooleanTypeIdentifier = "bool"

parseTypeIdentifier :: Parser ASTTypeIdentifier
parseTypeIdentifier =
    trace
        "parseTypeIdentifier"
        (
            choice
                [
                    try (
                        do
                            parseTokenInteger
                            return IntegerTypeIdentifier
                        ),
                    try (
                        do
                            parseTokenReal
                            return RealTypeIdentifier
                        ),
                    do
                        parseTokenBoolean
                        return BooleanTypeIdentifier
                    ]
            )

type ASTArrayType = (ASTSubrangeType, ASTTypeIdentifier)
parseArrayType :: Parser ASTArrayType
parseArrayType =
    trace
        "parseArrayType"
        (
            do
                parseTokenArray
                parseTokenLeftBracket
                x0 <-
                    parseSubrangeType
                parseTokenRightBracket
                parseTokenOf
                x1 <-
                    parseTypeIdentifier
                return (x0, x1)
            )

type ASTSubrangeType = (ASTConstant, ASTConstant)
parseSubrangeType :: Parser ASTSubrangeType
parseSubrangeType =
    trace
        "parseSubrangeType"
        (
            do
                x0 <-
                    parseConstant
                parseTokenEllipsis
                x1 <-
                    parseConstant
                return (x0, x1)
            )

type ASTConstant = ((Maybe ASTSign), ASTUnsignedInteger)
parseConstant :: Parser ASTConstant
parseConstant =
    trace
        "parseConstant"
        (
            do
                x0 <-
                    optionMaybe (
                        try (
                            parseSign
                            )
                        )
                x1 <-
                    parseUnsignedInteger
                return (x0, x1)
            )

type ASTSign = Sign
data Sign =
    SignPlus |
    SignMinus
    deriving(Eq, Show)
parseSign :: Parser ASTSign
parseSign =
    trace
        "parseSign"
        (
            choice
                [
                    try (
                        do
                            parseTokenPlus
                            return SignPlus
                        ),
                    do
                        parseTokenMinus
                        return SignMinus
                    ]
            )
