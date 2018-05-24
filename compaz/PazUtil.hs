-- COMP90045 Stage 3, Team ComPaz
-- PazUtil.hs
-- Helper functions to help with parentheses removal in printer.

module PazUtil where

import PazParser(
    ASTExpression,
    ASTSimpleExpression,
    ASTTerm
    )

-- determines operator precedence and consequently, necessity of parentheses
data Context = Relational | Additive | Multiplicative | Atomic
                deriving (Eq, Ord, Show)

-- get lowest operator precedence that exists inside the expression
-- determined by tracing down the abstract syntax tree
exprLoContext :: ASTExpression -> Context
exprLoContext (_, Just _) = Relational
exprLoContext (simp, Nothing) = simpExprLoContext simp

simpExprLoContext :: ASTSimpleExpression -> Context
simpExprLoContext (_, _, _:_) = Additive
simpExprLoContext (_, term, []) = termLoContext term

termLoContext :: ASTTerm -> Context
termLoContext (_, _:_) = Multiplicative
termLoContext (_, _) = Atomic

-- determine whether parentheses is needed for expression
-- by comparing operator precedence level around the expression
-- and inside the expression
needParen :: ASTExpression -> Context -> Bool
needParen expr context = f context (exprLoContext expr)
    where
        f Atomic _ = False
        f _ Atomic = False
        f context exprContext = context > exprContext

-- determine operator precedence level that is carried over
-- to children of expression
exprChildContext :: ASTExpression -> Context -> Context
exprChildContext (_, Just _) _ = Relational
exprChildContext (_, Nothing) c = stronger Atomic c

simpExprChildContext :: ASTSimpleExpression -> Context -> Context
simpExprChildContext (_, _, _:_) _ = Additive
simpExprChildContext (_, _, []) c = stronger Atomic c

termChildContext :: ASTTerm -> Context -> Context
termChildContext (_, _:_) _ = Multiplicative
termChildContext (_, _) c = stronger Atomic c

-- determine the stronger operator precedence level
stronger :: Context -> Context -> Context
stronger Atomic c = c
stronger c Atomic = c
stronger c1 c2 = max c1 c2
