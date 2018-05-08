module Codegen where

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
    Symbols,
    initSymbols
    )

import Data.Map (
    Map,
    (!),
    insert)
import qualified Data.Map as Map

import Control.Monad

type Reg = Int
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
initState = State 1 [] initSymbols 0

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
getRegType r = Codegen (\(State r c symbols l) ->
    let
        (_, _, regMap) = symbols
        t = regMap ! r
    in
        (t, State r c symbols l)
    )

putRegType :: Reg -> ASTTypeDenoter -> Codegen ()
putRegType r t = Codegen (\(State r c symbols l) ->
    let
        (a, b, regMap) = symbols
        s' = (a, b, insert r t regMap)
    in
        ((), State r c s' l)
    )

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
    writeCode "call main"
    writeCode "halt"
    writeCode "main:"
    cgCompoundStatement com
    writeCode "return"

cgCompoundStatement :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement [] = return ()
cgCompoundStatement (x:xs) = do
    cgStatement x
    cgCompoundStatement xs

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
    writeInstruction "call_builtin" ["print_string"]

-- cgProcedureStatement :: ASTProcedureStatement -> Codegen ()
-- cgProcedureStatement (id, maybeParams) = case id of
--     "writeln" -> case maybeParams of
--         Just [expr] -> (do
--             cgExpression expr regZero
--             writeInstruction "call_builtin print_string"
--             )
--         _ -> error ""
--     _ -> error ""

data NumTypeDenoter = IntegerOp | RealOp

getNumType :: ASTTypeDenoter -> NumTypeDenoter
getNumType (OrdinaryTypeDenoter ot) = case ot of
    BooleanTypeIdentifier   -> error "unexpected bool"
    IntegerTypeIdentifier   -> IntegerOp
    RealTypeIdentifier      -> RealOp
getNumType (ArrayTypeDenoter _) = error "unexpected array type"

cgIntToReal :: Reg -> Codegen ()
cgIntToReal r = writeInstruction "int_to_real" [showReg r, showReg r]

-- check types of both operands, do type casting if necessary, report final type
cgPrepareNumOp :: Reg -> Reg -> Codegen (NumTypeDenoter)
cgPrepareNumOp r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (getNumType t1, getNumType t2) of
        (RealOp, RealOp) -> return RealOp
        (IntegerOp, RealOp) -> do
            cgIntToReal r1
            return RealOp
        (RealOp, IntegerOp) -> do
            cgIntToReal r2
            return RealOp
        (IntegerOp, IntegerOp) -> return IntegerOp

cgExpression :: ASTExpression -> Reg -> Codegen ()
cgExpression (simpExpr, Nothing) dest =
    cgSimpleExpression simpExpr dest
cgExpression (e1, Just (relOp, e2)) dest = do
    r1 <- nextRegister
    r2 <- nextRegister
    cgSimpleExpression e1 r1
    cgSimpleExpression e2 r2
    ot <- cgPrepareNumOp r1 r2
    let a = case relOp of
            EqualDenoter -> "cmp_eq"
            NotEqualDenoter -> "cmp_ne"
            LessThanDenoter -> "cmp_lt"
            GreaterThanDenoter -> "cmp_gt"
            LessThanOrEqualDenoter -> "cmp_le"
            GreaterThanOrEqualDenoter -> "cmp_ge"
    let b = case ot of
            RealOp -> "real"
            IntegerOp -> "int"
    let cmd = a ++ "_" ++ b
    writeInstruction cmd [showReg dest, showReg r1, showReg r2]

cgSimpleExpression :: ASTSimpleExpression -> Reg -> Codegen ()
cgSimpleExpression (ms, t, post) dest = do
    r <- nextRegister
    cgTerm t r
    cgSimpleExpression' (ms, r, post) dest

cgSimpleExpression'
    :: (Maybe PazParser.ASTSign, Reg, [ASTPostTermModifier]) -> Reg -> Codegen ()
cgSimpleExpression' (Just PazParser.SignMinus, r, []) dest = do
    t <- getRegType r
    let cmd = case getNumType t of
            RealOp -> "neg_real"
            IntegerOp -> "neg_int"
    writeInstruction cmd [showReg r]
cgSimpleExpression' (_, r, []) dest =
    writeInstruction "move" [showReg dest, showReg r]
cgSimpleExpression' (ms, r1, ((addOp, t2):xs)) dest = do
    r2 <- nextRegister
    cgTerm t2 r2
    ot <- cgPrepareNumOp r1 r2
    let a = case addOp of
            PlusDenoter -> "add"
            MinusDenoter -> "sub"
            OrDenoter -> error "" -- TODO
    let b = case ot of
            IntegerOp -> "int"
            RealOp -> "real"
    let cmd = a ++ "_" ++ b
    r3 <- nextRegister
    writeInstruction cmd [showReg r3, showReg r1, showReg r2]
    cgSimpleExpression' (ms, r3, xs) dest


cgTerm :: ASTTerm -> Reg -> Codegen ()
cgTerm (factor, post) dest = do
    r <- nextRegister
    cgFactor factor r
    cgTerm' (r, post) dest

cgTerm' :: (Reg, [ASTPostFactorModifier]) -> Reg -> Codegen ()
cgTerm' (r, []) dest = writeInstruction "move" [showReg dest, showReg r]
cgTerm' (r1, (mulOp, f2):xs) dest = do
    r2 <- nextRegister
    cgFactor f2 r2
    let a = case mulOp of
            TimesDenoter -> "mul"
            DivideByDenoter -> "div"
            DivDenoter -> error "" -- TODO
            AndDenoter -> error "" -- TODO
    ot <- cgPrepareNumOp r1 r2
    let b = case ot of
            IntegerOp -> "int"
            RealOp -> "real"
    let cmd = a ++ "_" ++ b
    r3 <- nextRegister
    writeInstruction cmd [showReg r3, showReg r1, showReg r2]
    cgTerm' (r1, xs) dest

cgFactor :: ASTFactor -> Reg -> Codegen ()
cgFactor factor dest = case factor of
    UnsignedConstantDenoter c -> cgUnsignedConstant c dest
    VariableAccessDenoter var -> cgVariableAccess var dest
    ExpressionDenoter expr -> cgExpression expr dest
    NegatedFactorDenoter factor -> do
        cgFactor factor dest
        t <- getRegType dest
        let cmd = case getNumType t of
                RealOp -> "neg_real"
                IntegerOp -> "neg_int"
        writeInstruction cmd [showReg dest]

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
cgUnsignedInteger int dest =
    writeInstruction "int_const" [showReg dest, int]

cgUnsignedReal :: ASTUnsignedReal -> Reg -> Codegen ()
cgUnsignedReal (seq, maybeSeq, maybeScale) dest =
    let
        f1 = read seq :: Float
        f2 = case maybeSeq of
            Just s  -> f1 + read ("0." ++ s) :: Float
            Nothing -> f1
        scale = case maybeScale of
            Just (Nothing, s)
                -> (read s :: Int)
            Just (Just PazLexer.SignPlus, s)
                -> (read s :: Int)
            Just (Just PazLexer.SignMinus, s)
                -> -(read s :: Int)
            Nothing -> 1
        f3 = f2 * (10 ^ scale)
        regPart = showReg dest
        realPart = show f3
    in
        writeInstruction "real_const" [regPart, realPart]

cgBooleanConstant :: ASTBooleanConstant -> Reg -> Codegen ()
cgBooleanConstant bool dest =
    let
        val = case bool of
            FalseDenoter    -> 0
            TrueDenoter     -> 1
        regPart = showReg dest
        boolPart = show val
    in
        writeInstruction "int_const" [regPart, boolPart]