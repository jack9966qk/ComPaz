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

import Control.Monad

type Reg = Int
type Code = [Instruction]
type Instruction = String

data State = State Reg Code
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
initState = State 1 []

regZero :: Reg
regZero = 0

nextRegister :: Codegen Reg
nextRegister = Codegen (\(State r c) -> (r + 1, State (r + 1) c))

writeInstruction :: Instruction -> Codegen ()
writeInstruction inst = 
    Codegen (\(State r code) -> ((), State r (code ++ [inst])))

printSepBy :: IO () -> [IO ()] -> IO ()
printSepBy _ [] = return ()
printSepBy _ [x] = x
printSepBy sep (x:y:zs) =
    x >> sep >> (printSepBy sep (y:zs))

generateCode :: ASTProgram -> IO ()
generateCode prog = do
    let Codegen fn = cgProgram prog
    let (_, finalState) = fn initState
    let State _ instructions = finalState
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
    ProcedureStatementDenoter s -> cgProcedureStatement s
    _ -> error ""

cgProcedureStatement :: ASTProcedureStatement -> Codegen ()
cgProcedureStatement (id, maybeParams) = case id of
    "writeln" -> case maybeParams of
        Just [expr] -> (do
            cgExpression expr regZero
            writeInstruction "call_builtin print_string"
            )
        _ -> error ""
    _ -> error ""

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
    _ -> error ""

cgUnsignedConstant :: ASTUnsignedConstant -> Reg -> Codegen ()
cgUnsignedConstant const dest = case const of
    CharacterStringDenoter str -> cgCharacterString str dest
    _ -> error ""

cgCharacterString :: ASTCharacterString -> Reg -> Codegen ()
cgCharacterString str dest =
    let
        regPart = "r" ++ show dest
        strPart = "'" ++ str ++ "'"
    in
        writeInstruction $ "string_const " ++ regPart ++ " " ++ strPart