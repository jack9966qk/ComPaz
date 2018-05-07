module Sketch where

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

writeInstruction :: Instruction -> Codegen ()
writeInstruction inst = Codegen (\(State r code s l)
    -> ((), State r (code ++ [inst]) s l))

showReg :: Reg -> String
showReg r = "r" ++ show r

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
    writeInstruction "call main"
    writeInstruction "halt"
    writeInstruction "main:"
    cgCompoundStatement com

cgCompoundStatement :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement [] = return ()
cgCompoundStatement (x:xs) = do
    cgStatement x
    cgCompoundStatement xs

cgStatement :: ASTStatement -> Codegen ()
cgStatement stmt = case stmt of
    WriteStatementDenoter s -> cgWriteStatement s
    -- ProcedureStatementDenoter s -> cgProcedureStatement s
    _ -> error ""

cgWriteStatement :: ASTWriteStatement -> Codegen ()
cgWriteStatement expr = do
    cgExpression expr regZero
    writeInstruction "call_builtin print_string"

-- cgProcedureStatement :: ASTProcedureStatement -> Codegen ()
-- cgProcedureStatement (id, maybeParams) = case id of
--     "writeln" -> case maybeParams of
--         Just [expr] -> (do
--             cgExpression expr regZero
--             writeInstruction "call_builtin print_string"
--             )
--         _ -> error ""
--     _ -> error ""

cgExpression :: ASTExpression -> Reg -> Codegen ()
cgExpression (simpExpr, _) dest =
    cgSimpleExpression simpExpr dest

cgSimpleExpression :: ASTSimpleExpression -> Reg -> Codegen ()
cgSimpleExpression (_, term, _) dest = cgTerm term dest

cgTerm :: ASTTerm -> Reg -> Codegen ()
cgTerm (factor, _) dest = cgFactor factor dest

cgFactor :: ASTFactor -> Reg -> Codegen ()
cgFactor factor dest = case factor of
    UnsignedConstantDenoter c -> cgUnsignedConstant c dest
    -- VariableAccessDenoter var -> 
    -- ExpressionDenoter expr ->
    -- NegatedFactorDenoter factor ->
    _ -> error ""

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
        writeInstruction $ strJoinSpace ["string_const", regPart, strPart]

cgUnsignedNumber :: ASTUnsignedConstant -> Reg -> Codegen ()
cgUnsignedNumber num dest = case num of
    UnsignedIntegerDenoter i    -> cgUnsignedInteger i dest
    UnsignedRealDenoter r       -> cgUnsignedReal r dest
    BooleanConstantDenoter b    -> cgBooleanConstant b dest

cgUnsignedInteger :: ASTUnsignedInteger -> Reg -> Codegen ()
cgUnsignedInteger int dest =
    writeInstruction $ strJoinSpace ["int_const", showReg dest, int]

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
        writeInstruction $ strJoinSpace ["real_const", regPart, realPart]

cgBooleanConstant :: ASTBooleanConstant -> Reg -> Codegen ()
cgBooleanConstant bool dest =
    let
        val = case bool of
            FalseDenoter    -> 0
            TrueDenoter     -> 1
        regPart = showReg dest
        boolPart = show val
    in
        writeInstruction $ strJoinSpace ["int_const", regPart, boolPart]