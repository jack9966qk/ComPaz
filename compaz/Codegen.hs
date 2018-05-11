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
    lookupRegType,
    insertVariable,
    lookupVariable,
    insertArrayBounds,
    lookupArrayBounds
    )

import qualified Data.Map as Map

import Control.Monad

type StackSlot = Int
type Label = String
type LabelCounter = Int
type MemSize = Int
type Code = [Instruction]
type Instruction = String

data State = State Reg Code Symbols StackSlot LabelCounter
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
initState = State 0 [] initSymbols (-1) (-1)

regZero :: Reg
regZero = 0

nextRegister :: Codegen Reg
nextRegister = Codegen (\(State r c s sl l)
    -> (r + 1, State (r + 1) c s sl l))

nextSlot :: Codegen StackSlot
nextSlot = Codegen (\(State r c s sl l)
    -> (sl + 1, State r c s (sl + 1) l))


-- "allocate" a chunk of stackslots given size
-- return the starting stack slot position
nextSlotMulti :: MemSize -> Codegen StackSlot
nextSlotMulti n
    | n <= 0 = error ""
    | n == 1 = nextSlot
    | n > 1 = do
        sl <- nextSlot
        nextSlotMulti $ n - 1
        return sl
    
nextLabelCounter :: Codegen LabelCounter
nextLabelCounter = Codegen (\(State r c s sl l)
    -> (l + 1, State r c s sl (l + 1)))

nextLabel :: Codegen Label
nextLabel = nextLabelCounter >>= (\l -> return ("label" ++ show l))

writeCode :: Instruction -> Codegen ()
writeCode inst = Codegen (\(State r code s sl l)
    -> ((), State r (code ++ [inst]) s sl l))

writeComment :: String -> Codegen ()
writeComment str = writeCode $ "    # " ++ str

writeLabel :: Label -> Codegen ()
writeLabel str = writeCode $ str ++ ":"

writeInstruction :: String -> [String] -> Codegen ()
writeInstruction name [] = writeCode name
writeInstruction name args =
    writeCode $ "    " ++ name ++ " " ++ (strJoin ", " args)

showReg :: Reg -> String
showReg r = "r" ++ show r

getRegType :: Reg -> Codegen (ASTTypeDenoter)
getRegType reg = Codegen (\(State r c symbols sl l) ->
    (lookupRegType reg symbols, State r c symbols sl l))

putRegType :: Reg -> ASTTypeDenoter -> Codegen ()
putRegType reg typ = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertRegType reg typ symbols) sl l))

getVariable :: String -> Codegen (Bool, ASTTypeDenoter, Int)
getVariable name = Codegen (\(State r c symbols sl l) ->
    (lookupVariable name symbols, State r c symbols sl l))

putVariable :: String -> (Bool, ASTTypeDenoter, Int) -> Codegen ()
putVariable name val = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertVariable name val symbols) sl l))

getArrayBounds :: String -> Codegen (Int, Int)
getArrayBounds name = Codegen (\(State r c symbols sl l) ->
    (lookupArrayBounds name symbols, State r c symbols sl l))

putArrayBounds :: String -> (Int, Int) -> Codegen ()
putArrayBounds name val = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertArrayBounds name val symbols) sl l))

strJoin :: String -> [String] -> String
strJoin _ [] = ""
strJoin _ [x] = x
strJoin sep (x:y:zs) = x ++ sep ++ (strJoin sep (y:zs))

cgJoin :: [Codegen ()] -> Codegen ()
cgJoin [] = return ()
cgJoin (x:xs) = x >> (cgJoin xs)

cgFoldr :: (a -> b -> b) -> b -> [Codegen (a)] -> Codegen (b)
cgFoldr _ val [] = return val
cgFoldr fn val (x:xs) = do
    v <- x
    let val' = fn v val
    cgFoldr fn val' xs

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
    let State _ instructions _ _ _ = finalState
    printSepBy (putStr "\n") (map putStr instructions)

cgProgram :: ASTProgram -> Codegen ()
cgProgram (_, var, _, com) = do
    writeComment "program"
    size <- cgVariableDeclarationPart var
    writeCode "    call main"
    writeCode "    halt"
    writeLabel "main"
    cgPushStackFrame size
    cgCompoundStatement com
    cgPopStackFrame size
    writeCode "    return"

cgPushStackFrame :: MemSize -> Codegen ()
cgPushStackFrame size =
    writeInstruction "push_stack_frame" [show size]

cgPopStackFrame :: MemSize -> Codegen ()
cgPopStackFrame size =
    writeInstruction "pop_stack_frame" [show size]
    
cgVariableDeclarationPart :: ASTVariableDeclarationPart -> Codegen (MemSize)
cgVariableDeclarationPart var = do
    writeComment "variable declaration part"
    cgVariableDeclarationPart' var

cgVariableDeclarationPart' :: ASTVariableDeclarationPart -> Codegen (MemSize)
cgVariableDeclarationPart' Nothing = return 0
cgVariableDeclarationPart' (Just (decl, more)) = do
    cgFoldr (+) 0 $ map cgVariableDeclaration (decl:more)

cgVariableDeclaration :: ASTVariableDeclaration -> Codegen (MemSize)
cgVariableDeclaration ((ident, moreIdent), typ) = do
    writeComment "variable declaration"
    let cgDecl i = do
        sl <- nextSlot
        case typ of
            ArrayTypeDenoter arrayType -> cgArrayType i arrayType
            _ -> do
                putVariable i (True, typ, sl)
                return 1 -- all primitives have size 1
    cgFoldr (+) 0 $ map cgDecl (ident:moreIdent)

cgArrayType :: ASTIdentifier -> ASTArrayType -> Codegen (MemSize)
cgArrayType ident arrayType@((lo, hi), typeId) = do
    let readConst (maybeSign, uint) = case maybeSign of
            Just PazParser.SignMinus -> -(read uint :: Int)
            _ -> read uint :: Int
    let boundLo = readConst lo
    let boundHi = readConst hi
    let size = boundHi - boundLo + 1
    sl <- nextSlotMulti size
    putVariable ident (True, ArrayTypeDenoter arrayType, sl)
    putArrayBounds ident (boundLo, boundHi)
    return (size * 1)

cgCompoundStatement :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement stmt = do
    writeComment "compound statement"
    cgCompoundStatement' stmt

cgCompoundStatement' :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement' [] = return ()
cgCompoundStatement' (x:xs) = do
    cgStatement x
    cgCompoundStatement' xs

cgStatement :: ASTStatement -> Codegen ()
cgStatement stmt = case stmt of
    AssignmentStatementDenoter s -> cgAssignmentStatement s
    ReadStatementDenoter s -> cgReadStatement s
    WriteStatementDenoter e -> cgWriteStatement e
    WriteStringStatementDenoter s -> cgWriteStringStatement s
    WritelnStatementDenoter _ -> cgWriteln
    -- ProcedureStatementDenoter s -> cgProcedureStatement s
    CompoundStatementDenoter s -> cgCompoundStatement s
    IfStatementDenoter s -> cgIfStatement s
    WhileStatementDenoter s -> cgWhileStatement s
    -- ForStatementDenoter s -> cgForStatement s
    -- ProcedureStatementDenoter s -> cgProcedureStatement s
    EmptyStatementDenoter -> return ()
    _ -> error ""

cgIfStatement :: ASTIfStatement -> Codegen ()
cgIfStatement (expr, ifStmt, maybeElse) = do
    writeComment "if statement"
    elseLabel <- nextLabel
    afterLabel <- nextLabel
    r <- nextRegister
    cgExpression expr r
    writeInstruction "branch_on_false" [showReg r, elseLabel]
    cgStatement ifStmt
    writeInstruction "branch_uncond" [afterLabel]
    writeLabel elseLabel
    case maybeElse of
        Nothing -> return ()
        Just elseStmt -> cgStatement elseStmt
    writeLabel afterLabel

cgWhileStatement :: ASTWhileStatement -> Codegen ()
cgWhileStatement (expr, stmt) = do
    writeComment "while statement"
    beginLabel <- nextLabel
    afterLabel <- nextLabel
    writeLabel beginLabel
    r <- nextRegister
    cgExpression expr r
    writeInstruction "branch_on_false" [showReg r, afterLabel]
    cgStatement stmt
    writeInstruction "branch_uncond" [beginLabel]
    writeLabel afterLabel

cgPrepareAssignment :: ASTTypeDenoter -> (Reg, ASTTypeDenoter) -> Codegen ()
cgPrepareAssignment
    (OrdinaryTypeDenoter RealTypeIdentifier)
    (r, OrdinaryTypeDenoter IntegerTypeIdentifier) =
        cgIntToReal r
cgPrepareAssignment vt (r, et)
    | vt == et  = return ()
    | otherwise = error ""

cgAssignmentStatement :: ASTAssignmentStatement -> Codegen ()
cgAssignmentStatement (var, expr) = do
    r <- nextRegister
    cgExpression expr r
    et <- getRegType r
    (_, vt, addr) <- cgVariableAccess var
    cgPrepareAssignment vt (r, et)
    case addr of
        Direct sl
            -> writeInstruction "store" [show sl, showReg r]
        Indirect reg
            -> writeInstruction "store_indirect" [showReg reg, showReg r]

cgReadStatement :: ASTVariableAccess -> Codegen ()
cgReadStatement var = do
    (_, t, addr) <- cgVariableAccess var
    let name = case t of
            ArrayTypeDenoter _ -> error ""
            OrdinaryTypeDenoter IntegerTypeIdentifier -> "read_int"
            OrdinaryTypeDenoter RealTypeIdentifier -> "read_real"
            OrdinaryTypeDenoter BooleanTypeIdentifier -> "read_bool"
    writeInstruction "call_builtin" [name]
    case addr of
        Direct sl
            -> writeInstruction "store" [show sl, showReg regZero]
        Indirect reg
            -> writeInstruction "store_indirect" [showReg reg, showReg regZero]

-- the address of a variable access can either be
-- direct (with int representing stackslot), or
-- indirect for arrays (address stored in reg)
data VarAddress = Direct Int | Indirect Reg
cgVariableAccess :: ASTVariableAccess -> Codegen (Bool, ASTTypeDenoter, VarAddress)
cgVariableAccess (IndexedVariableDenoter (ident, expr)) = do
    (varness, typ, start) <- getVariable ident
    r <- nextRegister
    cgExpression expr r
    exprTyp <- getRegType r
    case (typ, exprTyp) of
        (
            ArrayTypeDenoter (_, t),
            OrdinaryTypeDenoter IntegerTypeIdentifier) -> do
                -- TODO Array bound checking could be performed here
                -- calculate the address for array element
                (lo, _) <- getArrayBounds ident
                r1 <- nextRegister
                r2 <- nextRegister
                writeInstruction "int_const" [showReg r1, show start]
                writeInstruction "int_const" [showReg r2, show lo]
                writeInstruction "sub_int" [showReg r, showReg r, showReg r2]
                writeInstruction "add_int" [showReg r, showReg r, showReg r1]
                return (varness, OrdinaryTypeDenoter t, Indirect r)
        _ -> error ""
cgVariableAccess (IdentifierDenoter ident) = do
    (varness, typ, slot) <- getVariable ident
    return (varness, typ, Direct slot)

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
    putRegType r astTypeReal

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

cgPrepareDivideBy :: Reg -> Reg -> Codegen ()
cgPrepareDivideBy dest r = do
    cgPrepareArithmetic dest r
    return ()

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

cgDivideBy :: Reg -> Reg -> Codegen ()
cgDivideBy dest r = do
    cgPrepareDivideBy dest r
    writeInstruction "div_real" [showReg dest, showReg dest, showReg r]
    putRegType dest astTypeReal

cgSimpleExpression :: ASTSimpleExpression -> Reg -> Codegen ()
cgSimpleExpression expr dest = do
    writeComment "SimpleExpression"
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
    writeComment "term"
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
            DivideByDenoter -> cgDivideBy dest r
            DivDenoter -> cgDiv dest r
            AndDenoter -> cgLogical dest r "and"

cgFactor :: ASTFactor -> Reg -> Codegen ()
cgFactor factor dest = case factor of
    UnsignedConstantDenoter c -> cgUnsignedConstant c dest
    VariableAccessDenoter var -> do
        (_, t, addr) <- cgVariableAccess var
        case addr of
            Direct sl
                -> writeInstruction "load" [showReg dest, show sl]
            Indirect reg
                -> writeInstruction "load_indirect" [showReg dest, showReg reg]
        putRegType dest t
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