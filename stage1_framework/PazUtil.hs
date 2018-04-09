module PazUtil where

import PazParser(
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

-- determines operator precedence and consequently, necessity of parentheses
data Context = Relational | Additive | Multiplicative | Atomic
                deriving (Eq, Ord, Show)

exprLoContext :: ASTExpression -> Context
exprLoContext (_, Just _) = Relational
exprLoContext (simp, Nothing) = simpExprLoContext simp

simpExprLoContext :: ASTSimpleExpression -> Context
simpExprLoContext (_, _, _:_) = Additive
simpExprLoContext (_, term, []) = termLoContext term

termLoContext :: ASTTerm -> Context
termLoContext (_, _:_) = Multiplicative
termLoContext (_, _) = Atomic

needParen :: ASTExpression -> Context -> Bool
needParen expr context = f context (exprLoContext expr)
    where
        f Atomic _ = False
        f _ Atomic = False
        f context exprContext = context > exprContext

exprChildContext :: ASTExpression -> Context -> Context
exprChildContext (_, Just _) _ = Relational
exprChildContext (_, Nothing) c = stronger Atomic c

simpExprChildContext :: ASTSimpleExpression -> Context -> Context
simpExprChildContext (_, _, _:_) _ = Additive
simpExprChildContext (_, _, []) c = stronger Atomic c

termChildContext :: ASTTerm -> Context -> Context
termChildContext (_, _:_) _ = Multiplicative
termChildContext (_, _) c = stronger Atomic c

stronger :: Context -> Context -> Context
stronger Atomic c = c
stronger c Atomic = c
stronger c1 c2 = max c1 c2
