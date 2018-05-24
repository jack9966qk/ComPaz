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
    lookupArrayBounds,
    insertProcedure,
    lookupProcedure,
    clearVariables
    )

import qualified Data.Map as Map

import Control.Monad

type StackSlot = Int
type Label = String
type LabelCounter = Int
type MemSize = Int
type Code = [Instruction]
type Instruction = String
type Stack = [Int]

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

setRegister :: Reg -> Codegen ()
setRegister r' = Codegen (\(State r c s sl l)
    -> ((), State r' c s sl l))

resetRegister :: Codegen Reg
resetRegister = Codegen (\(State r c s sl l)
    -> (0, State 0 c s sl l))

resetStack :: Codegen StackSlot
resetStack = Codegen (\(State r c s sl l)
    -> ((-1), State r c s (-1) l))

regZero :: Reg
regZero = 0

nextRegister :: Codegen Reg
nextRegister = Codegen (\(State r c s sl l)
    -> (r + 1, State (r + 1) c s sl l))

nextSlot :: Codegen StackSlot
nextSlot = Codegen (\(State r c s sl l)
    -> (sl + 1, State r c s (sl + 1) l))

-- "allocate" a chunk of stackslots given size
-- return the final stack slot position
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

resetVariables :: Codegen ()
resetVariables = Codegen (\(State r c symbols sl l) ->
    ((), State r c (clearVariables symbols) sl l))

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

putProcedure :: String -> [(Bool, ASTTypeDenoter)] -> Codegen ()
putProcedure name params = Codegen (\(State r c symbols sl l) ->
    ((), State r c (insertProcedure name params symbols) sl l))

getProcedure :: String -> Codegen ([(Bool, ASTTypeDenoter)])
getProcedure name = Codegen (\(State r c symbols sl l) ->
    (lookupProcedure name symbols, State r c symbols sl l))

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
cgProgram (_, var, proc, com) = do
    writeComment "program"
    writeCode "    call main"
    writeCode "    halt"
    cgPrepareAllProcedures proc
    cgProcedureDeclarationPart proc
    resetStack
    size <- cgVariableDeclarationPart var
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
    cgVariableDeclarationPart' False var

cgVariableDeclarationPart' :: Bool -> ASTVariableDeclarationPart -> Codegen (MemSize)
cgVariableDeclarationPart' _ Nothing = return 0
cgVariableDeclarationPart' _ (Just (decl, more)) = do
    cgFoldr (+) 0 $ map (cgVariableDeclaration False) (decl:more)

cgVariableDeclaration :: Bool -> ASTVariableDeclaration -> Codegen (MemSize)
cgVariableDeclaration varness ((ident, moreIdent), typ) = do
    writeComment ("variable declaration " ++ ident)
    case varness of
        True -> do
            let cgDecl i = do
                case typ of
                    OrdinaryTypeDenoter _ -> do
                        sl <- nextSlot
                        -- writeComment (show sl)
                        putVariable i (True, typ, sl)
                        return 1
                    _ -> return 0
            cgFoldr (+) 0 $ map cgDecl (ident:moreIdent)
        False -> do
            let cgDecl i = do
                case typ of
                    ArrayTypeDenoter arrayType -> cgArrayType i arrayType
                    _ -> do
                        sl <- nextSlot
                        -- writeComment (show sl)
                        -- all vars in declaration are used "by value"
                        putVariable i (False, typ, sl) 
                        return 1 -- all primitives have size 1
            cgFoldr (+) 0 $ map cgDecl (ident:moreIdent)

cgFormalParameterList :: ASTFormalParameterList -> Codegen (MemSize)
cgFormalParameterList (s, ss) = do
    writeComment "formal parameter section"
    let cgProcessSection (varness, ids, t) =
            cgVariableDeclaration varness (ids, t)
    cgFoldr (+) 0 $ map cgProcessSection (s:ss)

cgPrepareAllProcedures :: ASTProcedureDeclarationPart -> Codegen ()
cgPrepareAllProcedures ps = cgJoin $ map cgPrepareProcedure ps

cgProcedureDeclarationPart :: ASTProcedureDeclarationPart -> Codegen ()
cgProcedureDeclarationPart ps = do
    writeComment "procedure declaration part"
    cgProcedureDeclarationPart' ps

cgProcedureDeclarationPart' :: ASTProcedureDeclarationPart -> Codegen ()
cgProcedureDeclarationPart' ps = cgJoin $ map cgProcedureDeclaration ps

bareParameters :: [ASTFormalParameterSection] -> [(Bool, ASTTypeDenoter)]
bareParameters ss = map (\(x, _, d) -> (x, d)) ss

cgStoreArg :: Reg -> StackSlot -> [ASTFormalParameterSection] -> Codegen ()
cgStoreArg _ _ [] = return ()
cgStoreArg r sl (_:xs) = do
    writeInstruction "store" [show sl, showReg r]
    cgStoreArg (r+1) (sl+1) xs

cgPrepareProcedure :: ASTProcedureDeclaration -> Codegen ()
cgPrepareProcedure (ident, maybeParam, _, _) = do
    case maybeParam of
        Just (s, ss) -> do
            putProcedure ident (bareParameters (s:ss))
        Nothing -> do
            putProcedure ident []

cgProcedureDeclaration :: ASTProcedureDeclaration -> Codegen ()
cgProcedureDeclaration (ident, maybeParam, v, com) = do
    writeComment "procedure declaration"
    resetVariables
    writeLabel ident
    resetStack
    case maybeParam of
        Just (s, ss) -> do
            size <- cgFormalParameterList (s, ss)
            size2 <- cgVariableDeclarationPart v
            cgPushStackFrame (size + size2)
            cgStoreArg 1 0 (s:ss)
            cgCompoundStatement com
            cgPopStackFrame (size + size2)
        Nothing -> do
            size <- cgVariableDeclarationPart v
            cgPushStackFrame size
            cgCompoundStatement com
            cgPopStackFrame size
    writeCode "    return"

cgArrayType :: ASTIdentifier -> ASTArrayType -> Codegen (MemSize)
cgArrayType ident arrayType@((lo, hi), typeId) = do
    let readConst (maybeSign, uint) = case maybeSign of
            Just PazParser.SignMinus -> -(read uint :: Int)
            _ -> read uint :: Int
    let boundLo = readConst lo
    let boundHi = readConst hi
    let size = boundHi - boundLo + 1
    sl <- nextSlotMulti size
    -- varness of array declaration is not important
    -- since they are used by reference anyway
    -- here, putVariable stores the slot number of the beginning of array
    putVariable ident (False, ArrayTypeDenoter arrayType, sl)
    putArrayBounds ident (boundLo, boundHi)
    return (size * 1)

cgCompoundStatement :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement stmt = do
    writeComment "compound statement"
    cgCompoundStatement' stmt

cgCompoundStatement' :: ASTCompoundStatement -> Codegen ()
cgCompoundStatement' [] = return ()
cgCompoundStatement' (x:xs) = do
    r <- resetRegister
    cgStatement x
    cgCompoundStatement' xs

cgStatement :: ASTStatement -> Codegen ()
cgStatement stmt = case stmt of
    AssignmentStatementDenoter s -> cgAssignmentStatement s
    ReadStatementDenoter s -> cgReadStatement s
    WriteStatementDenoter e -> cgWriteStatement e
    WriteStringStatementDenoter s -> cgWriteStringStatement s
    WritelnStatementDenoter _ -> cgWriteln
    ProcedureStatementDenoter s -> cgProcedureStatement s
    CompoundStatementDenoter s -> cgCompoundStatement s
    IfStatementDenoter s -> cgIfStatement s
    WhileStatementDenoter s -> cgWhileStatement s
    ForStatementDenoter s -> cgForStatement s
    EmptyStatementDenoter -> return ()

cgPrepareForStatement
    :: (ASTIdentifier, ASTExpression, ASTExpression) -> Codegen ()
cgPrepareForStatement (ident, fromExpr, toExpr) = do
    -- some unnecessary work here, but easier to implement
    r1 <- nextRegister
    r2 <- nextRegister
    cgExpression fromExpr r1
    cgExpression toExpr r2
    t1 <- getRegType r1
    t2 <- getRegType r2
    (_, t3, _) <- getVariable ident
    case (t1, t2, t3) of
        (OrdinaryTypeDenoter IntegerTypeIdentifier,
            OrdinaryTypeDenoter IntegerTypeIdentifier,
            OrdinaryTypeDenoter IntegerTypeIdentifier) -> return ()
        _ -> error "for statement heading should only contain integers"

cgForStatement :: ASTForStatement -> Codegen ()
cgForStatement (ident, fromExpr, toDownTo, toExpr, stmt) = do
    let idVarAccess = IdentifierDenoter ident
    let idFactor = VariableAccessDenoter idVarAccess
    cgPrepareForStatement (ident, fromExpr, toExpr)
    cgAssignmentStatement (idVarAccess, fromExpr)
    -- make condition for the loop to continue
    let relOp = case toDownTo of
            ToDenoter -> LessThanOrEqualDenoter
            DownToDenoter -> GreaterThanOrEqualDenoter
    let idSimpExpr = ((Nothing), (idFactor , []), [])
    let toSimpleExpr = ((Nothing), (ExpressionDenoter toExpr, []), [])
    let condExpr = (idSimpExpr, Just (relOp, toSimpleExpr))
    -- add increment/decrement update to end of stmt
    let oneTerm = (UnsignedConstantDenoter $ UnsignedIntegerDenoter "1", [])
    let varTerm = (idFactor, [])
    let addOp = case toDownTo of
            ToDenoter -> PlusDenoter
            DownToDenoter -> MinusDenoter
    let simpleExpr = ((Nothing), varTerm, [(addOp, oneTerm)])
    let updateExpr = (simpleExpr, Nothing)
    let updateStmt = AssignmentStatementDenoter (idVarAccess, updateExpr)
    let newStmt = CompoundStatementDenoter [stmt, updateStmt]
    -- convert to while loop
    cgWhileStatement (condExpr, newStmt)

cgIfStatement' :: ASTExpression -> Codegen () -> Maybe (Codegen ()) -> Codegen ()
cgIfStatement' expr ifCg maybeElse = do
    elseLabel <- nextLabel
    afterLabel <- nextLabel
    r <- nextRegister
    cgExpression expr r
    writeInstruction "branch_on_false" [showReg r, elseLabel]
    ifCg
    writeInstruction "branch_uncond" [afterLabel]
    writeLabel elseLabel
    case maybeElse of
        Nothing -> return ()
        Just elseCg -> elseCg
    writeLabel afterLabel

cgIfStatement :: ASTIfStatement -> Codegen ()
cgIfStatement (expr, ifStmt, maybeElse) = do
    writeComment "if statement"
    let maybeElseCg = maybeElse >>= (Just . cgStatement)
    cgIfStatement' expr (cgStatement ifStmt) maybeElseCg

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
cgPrepareAssignment vt (_, et)
    | vt == et  = return ()
    | otherwise = error $ "cannot assign " ++ (show et) ++ " to " ++ (show vt)

cgAssignmentStatement :: ASTAssignmentStatement -> Codegen ()
cgAssignmentStatement (var, expr) = do
    r <- nextRegister
    cgExpression expr r
    et <- getRegType r
    (vt, addr) <- cgVariableAccess var
    cgPrepareAssignment vt (r, et)
    case addr of
        Direct sl
            -> writeInstruction "store" [show sl, showReg r]
        Indirect reg 
            -> writeInstruction "store_indirect" [showReg reg, showReg r]

cgReadStatement :: ASTVariableAccess -> Codegen ()
cgReadStatement var = do
    t <- cgGetVariableType var
    let name = case t of
            ArrayTypeDenoter _ -> error "cannot read array"
            OrdinaryTypeDenoter IntegerTypeIdentifier -> "read_int"
            OrdinaryTypeDenoter RealTypeIdentifier -> "read_real"
            OrdinaryTypeDenoter BooleanTypeIdentifier -> "read_bool"
    writeInstruction "call_builtin" [name]
    (_, addr) <- cgVariableAccess var
    case addr of
        Direct sl
            -> writeInstruction "store" [show sl, showReg regZero]
        Indirect reg
            -> writeInstruction "store_indirect" [showReg reg, showReg regZero]

cgGetVariableType :: ASTVariableAccess -> Codegen (ASTTypeDenoter)
cgGetVariableType (IdentifierDenoter ident) = do
    (_, t, _) <- getVariable ident
    return t
cgGetVariableType (IndexedVariableDenoter (ident, expr)) = do
    (_, ArrayTypeDenoter (_, t), _) <- getVariable ident
    return $ OrdinaryTypeDenoter t

cgArrayBoundCheck :: ASTIdentifier -> ASTExpression -> Codegen ()
cgArrayBoundCheck ident expr = do
    writeComment "array bound check"
    -- construct and generate "if ( (expr < lo) or (expr > hi) ) then halt"
    (lo, hi) <- getArrayBounds ident
    let termFromExpr e = (ExpressionDenoter e, []) :: ASTTerm
    let simpFromExpr e = ((Nothing), termFromExpr e, []) :: ASTSimpleExpression
    let makeCompExpr e1 op e2 =
            ((simpFromExpr e1), Just (op, simpFromExpr e2)) :: ASTExpression
    let exprFromSimp s = (s, Nothing) :: ASTExpression
    let makeAddExpr e1 op e2 =
            exprFromSimp $ ((Nothing), termFromExpr e1, [(op, termFromExpr e2)])
    let exprFromInt i =
            let
                t = (UnsignedConstantDenoter $ UnsignedIntegerDenoter $ show i, []) :: ASTTerm
            in
                exprFromSimp ((Nothing), t, []) :: ASTExpression
    let loExpr = exprFromInt lo
    let hiExpr = exprFromInt hi
    let lessThanLo = makeCompExpr expr LessThanDenoter loExpr
    let greaterThanHi = makeCompExpr expr GreaterThanDenoter hiExpr
    let condExpr = makeAddExpr lessThanLo OrDenoter greaterThanHi
    -- instructions to execute if out of bound
    let cgOutOfBound = do
        cgWriteStringStatement $ "array access on " ++ ident ++ " out of range"
        cgWriteln
        cgWriteStringStatement $ "expected [" ++ (show lo) ++ ".." ++ (show hi) ++ "], received "
        cgWriteStatement expr
        cgWriteln
        writeInstruction "halt" []
    cgIfStatement' condExpr cgOutOfBound Nothing
    

-- the address of a variable access can either be
-- direct (with int representing stackslot), or
-- indirect for arrays (address stored in reg)
data VarAddress = Direct Int | Indirect Reg
cgVariableAccess :: ASTVariableAccess -> Codegen (ASTTypeDenoter, VarAddress)
cgVariableAccess (IndexedVariableDenoter (ident, expr)) = do
    -- varness of an array variable is not considered
    (_, typ, start) <- getVariable ident
    r <- nextRegister
    cgExpression expr r
    exprTyp <- getRegType r
    case (typ, exprTyp) of
        (
            ArrayTypeDenoter (_, t),
            OrdinaryTypeDenoter IntegerTypeIdentifier) -> do
                -- array bound checking performed here
                cgArrayBoundCheck ident expr
                -- calculate the address for array element
                (lo, _) <- getArrayBounds ident
                r1 <- nextRegister
                r2 <- nextRegister
                writeInstruction "load_address" [showReg r1, show start]
                writeInstruction "int_const" [showReg r2, show lo]
                writeInstruction "sub_int" [showReg r, showReg r, showReg r2]
                writeInstruction "sub_offset" [showReg r1, showReg r1, showReg r]
                -- return register that holds address to the array element
                return (OrdinaryTypeDenoter t, Indirect r1)
        (_, OrdinaryTypeDenoter IntegerTypeIdentifier)
            -> error $ "indexing of variable " ++ ident ++ " that is not array"
        (_, _) -> error $ "index of " ++ ident ++ " is not integer"
cgVariableAccess (IdentifierDenoter ident) = do
    -- writeComment ident
    (varness, typ, slot) <- getVariable ident
    if varness then do
        -- slot holds the address of variable
        -- load address to register and return
        r <- nextRegister
        writeInstruction "load" [showReg r, show slot]
        return (typ, Indirect r)
    else
        -- slot holds the value of variable
        return (typ, Direct slot)

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
            ArrayTypeDenoter _ -> error "cannot write array"
            OrdinaryTypeDenoter IntegerTypeIdentifier -> "print_int"
            OrdinaryTypeDenoter RealTypeIdentifier -> "print_real"
            OrdinaryTypeDenoter BooleanTypeIdentifier -> "print_bool"
    writeInstruction "call_builtin" [name]

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
        _ -> error $ "arithmetic/comparision cannot be done between " ++
                (show t1) ++ " and " ++ (show t2)

cgPrepareLogical :: Reg -> Reg -> Codegen ()
cgPrepareLogical r1 r2 = do
    t1 <- getRegType r1
    t2 <- getRegType r2
    case (t1, t2) of
        (
            OrdinaryTypeDenoter BooleanTypeIdentifier,
            OrdinaryTypeDenoter BooleanTypeIdentifier
            ) -> return ()
        _ -> error $ "logical operation cannot be done between " ++
                (show t1) ++ " and " ++ (show t2)

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
        _ -> error $ "integer division cannot be done between " ++
                (show t1) ++ " and " ++ (show t2)

cgPrepareDivideBy :: Reg -> Reg -> Codegen ()
cgPrepareDivideBy dest r = do
    t <- cgPrepareArithmetic dest r
    case t of
        IntOp -> do
            cgIntToReal dest
            cgIntToReal r
        RealOp -> return ()

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
cgSimpleExpression' (maybeSign, term, modifiers) dest = do
    cgTerm term dest
    case maybeSign of
        Just PazParser.SignMinus -> do
            t <- getRegType dest
            let cmd = case t of
                    OrdinaryTypeDenoter RealTypeIdentifier -> "neg_real"
                    OrdinaryTypeDenoter IntegerTypeIdentifier -> "neg_int"
                    _ -> error "negation cannot be done with " ++ (show t)
            writeInstruction cmd [showReg dest, showReg dest]
        _ -> return ()
    let cgOne (op, t) = do
        r <- nextRegister
        cgTerm t r
        case op of
            PlusDenoter -> cgArithmetic dest r "add"
            MinusDenoter -> cgArithmetic dest r "sub"
            OrDenoter -> cgLogical dest r "or"
    cgJoin $ map cgOne modifiers

cgTerm :: ASTTerm -> Reg -> Codegen ()
cgTerm term dest = do
    writeComment "term"
    cgTerm' term dest

cgTerm' :: ASTTerm -> Reg -> Codegen ()
cgTerm' (factor, modifiers) dest = do
    cgFactor factor dest
    let cgOne (op, f) = do
        r <- nextRegister
        cgFactor f r
        case op of
                TimesDenoter -> cgArithmetic dest r "mul"
                DivideByDenoter -> cgDivideBy dest r
                DivDenoter -> cgDiv dest r
                AndDenoter -> cgLogical dest r "and"
    cgJoin $ map cgOne modifiers

-- Bool indicates if caller passes Factor by reference
cgFactor :: ASTFactor -> Reg -> Codegen ()
cgFactor factor dest =
    case factor of
        UnsignedConstantDenoter c -> cgUnsignedConstant c dest
        VariableAccessDenoter var -> do
            (t, addr) <- cgVariableAccess var
            case addr of
                Direct sl
                    -> writeInstruction "load" [showReg dest, show sl]
                Indirect reg
                    -> writeInstruction "load_indirect" [showReg dest, showReg reg]
            putRegType dest t
        ExpressionDenoter expr -> cgExpression expr dest -- expr in expr
        NegatedFactorDenoter factor -> do
            cgFactor factor dest
            t <- getRegType dest
            case t of
                OrdinaryTypeDenoter BooleanTypeIdentifier
                    -> writeInstruction "not" [showReg dest, showReg dest]
                _ -> error $ "negation cannot be done with " ++ (show t)
            putRegType dest t

cgUnsignedConstant :: ASTUnsignedConstant -> Reg -> Codegen ()
cgUnsignedConstant const dest = case const of
    BooleanConstantDenoter bool -> cgBooleanConstant bool dest
    UnsignedIntegerDenoter int -> cgUnsignedInteger int dest
    UnsignedRealDenoter real -> cgUnsignedReal real dest

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

cgAllocateRegs :: [ASTExpression] -> Codegen ()
cgAllocateRegs [] = return ()
cgAllocateRegs (_:xs) = nextRegister >> (cgAllocateRegs xs)

cgPassArgument :: Reg -> [ASTExpression] -> [(Bool, ASTTypeDenoter)] -> Codegen ()
cgPassArgument _ (_:_) [] = error "num of arguments incorrect"
cgPassArgument _ [] (_:_) = error "num of arguments incorrect"
cgPassArgument _ [] [] = return ()
cgPassArgument r (a:as) ((v, vt):ps) = do
    if v then
        -- pass by reference
        cgVariableReference a r
    else do
        -- pass by value
        cgExpression a r
        at <- getRegType r
        cgPrepareAssignment vt (r, at)
    cgPassArgument (r+1) as ps

cgVariableReference :: ASTExpression -> Reg -> Codegen ()
cgVariableReference (
    (
        Nothing, (
            VariableAccessDenoter (
                IdentifierDenoter var
                ), []
            ), []
        ), Nothing
    ) dest = do
    (varness, _, sl) <- getVariable var
    case varness of
        True
            -> writeInstruction "load" [showReg dest, show sl]
        False
            -> writeInstruction "load_address" [showReg dest, show sl]
cgVariableReference _ _ = error $ "var argument must be passed as varaible"

cgProcedureStatement :: ASTProcedureStatement -> Codegen ()
cgProcedureStatement (p, paramList) = do
    formalParameters <- getProcedure p
    case paramList of
        Nothing -> return ()
        Just arguments -> do
            -- prepare registers
            resetRegister
            cgAllocateRegs arguments
            -- put in arguments
            cgPassArgument 1 arguments formalParameters
    writeInstruction "call" [p]