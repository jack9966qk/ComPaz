module Codegen where

import Debug.Trace (trace)

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
    ASTStatement,
    ASTStatementDenoter(..),
    ASTProcedureStatement,
    ASTActualParameterList,
    ASTIfStatement,
    ASTWhileStatement,
    ASTForStatement,
    ToDownToDenoter(..),
    ASTBooleanConstant,
    BooleanConstantDenoter(..),
    ASTWriteStatement,
    ASTWriteStringStatement,
    ASTPostTermModifier,
    ASTPostFactorModifier
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

import Symbol (
    Reg,
    Symbols,
    initSymbols,
    insertRegType,
    lookupRegType
    )

import qualified Data.Map as Map

import Control.Monad

type Label = Int
type Code = [Instruction]
type Instruction = String

data State = State Reg Code Symbols Label
data Codegen a = Codegen (State -> (a, State))
instance Monad Codegen where
    return x = Codegen (\s -> (x, s))
    Codegen f >>= fn = Codegen (\s ->
        let
            (x, s') = f s
            Codegen f' = fn x
        in
            f' s')

instance Functor Codegen where
    fmap = liftM

instance Applicative Codegen where
    pure = return
    (<*>) = ap

initState :: State
initState = State 0 [] initSymbols 0

regZero :: Reg
regZero = 0

nextRegister :: Codegen Reg
nextRegister = Codegen (\(State r c s l)
    -> (r + 1, State (r + 1) c s l))

nextLabel :: Codegen Label
nextLabel = Codegen (\(State r c s l)
    -> (l + 1, State r c s (l + 1)))

writeCode :: Instruction -> Codegen ()
writeCode inst = Codegen (\(State r code s l)
    -> ((), State r (code ++ [inst]) s l))
    

writeInstruction :: String -> [String] -> Codegen ()
writeInstruction name [] = writeCode name
writeInstruction name args =
    writeCode $ name ++ " " ++ (strJoin ", " args)

showReg :: Reg -> String
showReg r = "r" ++ show r

getRegType :: Reg -> Codegen (ASTTypeDenoter)
getRegType reg = Codegen (\(State r c symbols l) ->
    (lookupRegType reg symbols, State r c symbols l))

putRegType :: Reg -> ASTTypeDenoter -> Codegen ()
putRegType reg typ = Codegen (\(State r c symbols l) ->
    ((), State r c (insertRegType reg typ symbols) l))

strJoin :: String -> [String] -> String
strJoin _ [] = ""
strJoin _ [x] = x
strJoin sep (x:y:zs) = x ++ sep ++ (strJoin sep (y:zs))

strJoinSpace :: [String] -> String
strJoinSpace = strJoin " "

printSepBy :: IO () -> [IO ()] -> IO ()
printSepBy _ [] = return ()
printSepBy _ [x] = x
printSepBy sep (x:y:zs) =
    x >> sep >> (printSepBy sep (y:zs))

generateCode :: ASTProgram -> IO ()
generateCode prog = do
    let Codegen fn = cgProgram prog
    let (_, finalState) = fn initState
    let State _ instructions _ _ = finalState
    printSepBy (putStr "\n") (map putStr instructions)

cgProgram :: ASTProgram -> Codegen ()
cgProgram (_, _, _, com) = do
    writeCode "# program"
    writeCode "call main"
    writeCode "halt"
    writeCode "main:"
    cgCompoundStatement com
    writeCode "return"

cgCompoundStatement :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement stmt = do
    writeCode "# compound statement"
    cgCompoundStatement' stmt

cgCompoundStatement' :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement' [] = return ()
cgCompoundStatement' (x:xs) = do
    cgStatement x
    cgCompoundStatement' xs

cgStatement :: ASTStatement -> Codegen ()
cgStatement stmt = case stmt of
    -- AssignmentStatementDenoter s -> cgAssignmentStatement s
    -- ReadStatementDenoter s -> cgReadStatement s
    WriteStatementDenoter e -> cgWriteStatement e
    WriteStringStatementDenoter s -> cgWriteStringStatement s
    WritelnStatementDenoter _ -> cgWriteln
    -- ProcedureStatementDenoter s -> cgProcedureStatement s
    -- CompoundStatementDenoter s -> cgCompoundStatement s
    -- IfStatementDenoter s -> cgIfStatement s
    -- WhileStatementDenoter s -> cgWhileStatement s
    -- ForStatementDenoter s -> cgForStatement s
    -- ProcedureStatementDenoter s -> cgProcedureStatement s
    EmptyStatementDenoter -> return ()
    _ -> error ""

cgWriteln :: Codegen ()
cgWriteln = writeInstruction "call_builtin" ["print_newline"]

cgWriteStringStatement :: ASTWriteStringStatement -> Codegen ()
cgWriteStringStatement str = do
    cgCharacterString str regZero
    writeInstruction "call_builtin" ["print_string"]

cgWriteStatement :: ASTExpression -> Codegen ()
cgWriteStatement expr = do
    cgExpression expr regZero
    t <- getRegType regZero
    let name = case t of
            ArrayTypeDenoter _ -> error ""
            OrdinaryTypeDenoter IntegerTypeIdentifier -> "print_int"
            OrdinaryTypeDenoter RealTypeIdentifier -> "print_real"
            OrdinaryTypeDenoter BooleanTypeIdentifier -> "print_bool"
    writeInstruction "call_builtin" [name]

-- cgProcedureStatement :: ASTProcedureStatement -> Codegen ()
-- cgProcedureStatement (id, maybeParams) = case id of
--     "writeln" -> case maybeParams of
--         Just [expr] -> (do
--             cgExpression expr regZero
--             writeInstruction "call_builtin print_string"
--             )
--         _ -> error ""
--     _ -> error ""

astTypeInt :: ASTTypeDenoter
astTypeInt = OrdinaryTypeDenoter IntegerTypeIdentifier

astTypeReal :: ASTTypeDenoter
astTypeReal = OrdinaryTypeDenoter RealTypeIdentifier

astTypeBool :: ASTTypeDenoter
astTypeBool = OrdinaryTypeDenoter BooleanTypeIdentifier

cgIntToReal :: Reg -> Codegen ()
cgIntToReal r = do
    writeInstruction "int_to_real" [showReg r, showReg r]
    putRegType r (OrdinaryTypeDenoter RealTypeIdentifier)

data OperatorType = IntOp | RealOp

-- check types of both operands, do type casting if necessary, report final type
cgPrepareArithmetic :: Reg -> Reg -> Codegen (OperatorType)
cgPrepareArithmetic r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (
            OrdinaryTypeDenoter RealTypeIdentifier,
            OrdinaryTypeDenoter RealTypeIdentifier
            ) -> return RealOp
        (
            OrdinaryTypeDenoter IntegerTypeIdentifier,
            OrdinaryTypeDenoter RealTypeIdentifier
            ) -> do
                cgIntToReal r1
                return RealOp
        (
            OrdinaryTypeDenoter RealTypeIdentifier,
            OrdinaryTypeDenoter IntegerTypeIdentifier
            ) -> do
                cgIntToReal r2
                return RealOp
        (
            OrdinaryTypeDenoter IntegerTypeIdentifier,
            OrdinaryTypeDenoter IntegerTypeIdentifier
            ) -> return IntOp
        _   -> error ""

cgPrepareLogical :: Reg -> Reg -> Codegen ()
cgPrepareLogical r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (
            OrdinaryTypeDenoter BooleanTypeIdentifier,
            OrdinaryTypeDenoter BooleanTypeIdentifier
            ) -> return ()
        _   -> error ""

cgPrepareComparison :: Reg -> Reg -> Codegen (OperatorType)
cgPrepareComparison = cgPrepareArithmetic

cgPrepareDiv :: Reg -> Reg -> Codegen ()
cgPrepareDiv r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (
            OrdinaryTypeDenoter IntegerTypeIdentifier,
            OrdinaryTypeDenoter IntegerTypeIdentifier
            ) -> return ()
        _   -> error ""

cgPrepareDivideBy :: Reg -> Reg -> Codegen (OperatorType)
cgPrepareDivideBy = cgPrepareArithmetic

cgExpression :: ASTExpression -> Reg -> Codegen ()
cgExpression (simpExpr, Nothing) dest =
    cgSimpleExpression simpExpr dest
cgExpression (e1, Just (relOp, e2)) dest = do
    r1 <- nextRegister
    r2 <- nextRegister
    cgSimpleExpression e1 r1
    cgSimpleExpression e2 r2
    ot <- cgPrepareComparison r1 r2
    let a = case relOp of
            EqualDenoter -> "cmp_eq"
            NotEqualDenoter -> "cmp_ne"
            LessThanDenoter -> "cmp_lt"
            GreaterThanDenoter -> "cmp_gt"
            LessThanOrEqualDenoter -> "cmp_le"
            GreaterThanOrEqualDenoter -> "cmp_ge"
    let b = case ot of
            RealOp -> "real"
            IntOp -> "int"
    let cmd = a ++ "_" ++ b
    writeInstruction cmd [showReg dest, showReg r1, showReg r2]
    putRegType dest astTypeBool


cgMove :: Reg -> Reg -> Codegen ()
cgMove to from = do
    writeInstruction "move" [showReg from, showReg to]
    t <- getRegType from
    putRegType to t


cgArithmetic :: Reg -> Reg -> String -> Codegen ()
cgArithmetic dest r a = do
    ot <- cgPrepareArithmetic dest r
    let (b, t) = case ot of
            IntOp -> ("int", astTypeInt)
            RealOp -> ("real", astTypeReal)
    let cmd = a ++ "_" ++ b
    writeInstruction cmd [showReg dest, showReg dest, showReg r]
    putRegType dest t

cgLogical :: Reg -> Reg -> String -> Codegen ()
cgLogical dest r cmd = do
    cgPrepareLogical dest r
    writeInstruction cmd [showReg dest, showReg dest, showReg r]
    putRegType dest astTypeBool

cgDiv :: Reg -> Reg -> Codegen ()
cgDiv dest r = do
    cgPrepareDiv dest r
    writeInstruction "div_int" [showReg dest, showReg dest, showReg r]
    putRegType dest astTypeInt

cgSimpleExpression :: ASTSimpleExpression -> Reg -> Codegen ()
cgSimpleExpression expr dest = do
    writeCode "# SimpleExpression"
    cgSimpleExpression' expr dest

cgSimpleExpression' :: ASTSimpleExpression -> Reg -> Codegen ()
cgSimpleExpression' (Just PazParser.SignMinus, term, []) dest = do
    cgTerm term dest
    t <- getRegType dest
    let cmd = case t of
            OrdinaryTypeDenoter RealTypeIdentifier -> "neg_real"
            OrdinaryTypeDenoter IntegerTypeIdentifier -> "neg_int"
            _ -> error ""
    writeInstruction cmd [showReg dest]
cgSimpleExpression' (_, term, []) dest =
    cgTerm term dest
cgSimpleExpression' (ms, t1, x:xs) dest = do
    let (addOp, t2) = x
    cgTerm t1 dest
    r <- nextRegister
    cgTerm t2 r
    case addOp of
        PlusDenoter -> cgArithmetic dest r "add"
        MinusDenoter -> cgArithmetic dest r "sub"
        OrDenoter -> cgLogical dest r "or"

cgTerm :: ASTTerm -> Reg -> Codegen ()
cgTerm term dest = do
    writeCode "# term"
    cgTerm' term dest

cgTerm' :: ASTTerm -> Reg -> Codegen ()
cgTerm' (factor, []) dest = cgFactor factor dest
cgTerm' (f1, x:xs) dest = do
    let (mulOp, f2) = x
    cgFactor f1 dest
    r <- nextRegister
    cgFactor f2 r
    case mulOp of
            TimesDenoter -> cgArithmetic dest r "mul"
            DivideByDenoter -> error ""
            DivDenoter -> cgDiv dest r
            AndDenoter -> cgLogical dest r "or"

cgFactor :: ASTFactor -> Reg -> Codegen ()
cgFactor factor dest = case factor of
    UnsignedConstantDenoter c -> cgUnsignedConstant c dest
    VariableAccessDenoter var -> cgVariableAccess var dest
    ExpressionDenoter expr -> cgExpression expr dest
    NegatedFactorDenoter factor -> do
        cgFactor factor dest
        t <- getRegType dest
        let cmd = case t of
                OrdinaryTypeDenoter IntegerTypeIdentifier -> "neg_int"
                OrdinaryTypeDenoter RealTypeIdentifier -> "neg_real"
                _ -> error ""
        writeInstruction cmd [showReg dest]
        putRegType dest t

cgVariableAccess :: ASTVariableAccess -> Reg -> Codegen ()
cgVariableAccess var dest = error ""

cgUnsignedConstant :: ASTUnsignedConstant -> Reg -> Codegen ()
cgUnsignedConstant const dest = case const of
    BooleanConstantDenoter bool -> cgBooleanConstant bool dest
    UnsignedIntegerDenoter int -> cgUnsignedInteger int dest
    UnsignedRealDenoter real -> cgUnsignedReal real dest
    -- CharacterStringDenoter str -> cgCharacterString str dest
    -- UnsignedNumberDenoter num -> cgUnsignedNumber num dest

cgCharacterString :: ASTCharacterString -> Reg -> Codegen ()
cgCharacterString str dest =
    let
        regPart = showReg dest
        strPart = "'" ++ str ++ "'"
    in
        writeInstruction "string_const" [regPart, strPart]

cgUnsignedNumber :: ASTUnsignedConstant -> Reg -> Codegen ()
cgUnsignedNumber num dest = case num of
    UnsignedIntegerDenoter i    -> cgUnsignedInteger i dest
    UnsignedRealDenoter r       -> cgUnsignedReal r dest
    BooleanConstantDenoter b    -> cgBooleanConstant b dest

cgUnsignedInteger :: ASTUnsignedInteger -> Reg -> Codegen ()
cgUnsignedInteger int dest = do
    writeInstruction "int_const" [showReg dest, int]
    putRegType dest (OrdinaryTypeDenoter IntegerTypeIdentifier)

cgUnsignedReal :: ASTUnsignedReal -> Reg -> Codegen ()
cgUnsignedReal (seq, maybeSeq, maybeScale) dest = do
    let f1 = read seq :: Float
    let f2 = case maybeSeq of
            Just s  -> f1 + read ("0." ++ s) :: Float
            Nothing -> f1
    let scale = case maybeScale of
            Just (Nothing, s)
                -> (read s :: Int)
            Just (Just PazLexer.SignPlus, s)
                -> (read s :: Int)
            Just (Just PazLexer.SignMinus, s)
                -> -(read s :: Int)
            Nothing -> 0
    let f3 = f2 * (10 ^ scale)
    let regPart = showReg dest
    let realPart = show f3
    writeInstruction "real_const" [regPart, realPart]
    putRegType dest (OrdinaryTypeDenoter RealTypeIdentifier)

cgBooleanConstant :: ASTBooleanConstant -> Reg -> Codegen ()
cgBooleanConstant bool dest = do
    let val = case bool of
            FalseDenoter    -> 0
            TrueDenoter     -> 1
    let regPart = showReg dest
    let boolPart = show val
    writeInstruction "int_const" [regPart, boolPart]
    putRegType dest (OrdinaryTypeDenoter BooleanTypeIdentifier)