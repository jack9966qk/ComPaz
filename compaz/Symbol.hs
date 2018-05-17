-- Compaz Stage 3
-- Functionalities for Symbol Table

module Symbol where

import Debug.Trace (trace)

import Data.Map (
    Map,
    (!),
    insert
    )
import qualified Data.Map as Map
import PazParser (
    ASTTypeDenoter
    )

type Reg = Int

-- Adapted from Compiller.hs in provided stage 1 solution
type Symbols = (
    -- for each procedure, for each formal parameter, its varness and type
    Map String [(Bool, ASTTypeDenoter)],
    -- for each variable, its varness, type, and starting slot number
    Map String (Bool, ASTTypeDenoter, Int),
    -- type for each value in register
    Map Reg ASTTypeDenoter,
    -- for each array variable, its lower and upper bound
    Map String (Int, Int)
    )

initSymbols :: Symbols
initSymbols = (Map.empty, Map.empty, Map.empty, Map.empty)

insertRegType :: Reg -> ASTTypeDenoter -> Symbols -> Symbols
insertRegType r t (a, b, map, d) = -- trace (show $ insert r t map)
    (a, b, insert r t map, d)

lookupRegType :: Reg -> Symbols -> ASTTypeDenoter
lookupRegType r (_, _, map, _) = -- trace ( (show map) ++ " get " ++ (show r) )
    (map ! r)

insertVariable :: String -> (Bool, ASTTypeDenoter, Int) -> Symbols -> Symbols
insertVariable name val (a, map, c, d) =
    (a, insert name val map, c, d)

lookupVariable :: String -> Symbols -> (Bool, ASTTypeDenoter, Int)
lookupVariable name (_, map, _, _) = (map ! name)

insertArrayBounds :: String -> (Int, Int) -> Symbols -> Symbols
insertArrayBounds name val (a, b, c, map) =
    (a, b, c, insert name val map)

lookupArrayBounds :: String -> Symbols -> (Int, Int)
lookupArrayBounds name (_, _, _, map) = (map ! name)

insertProcedure :: String -> [(Bool, ASTTypeDenoter)] -> Symbols -> Symbols
insertProcedure name vals (map, b, c, d) =
    (insert name vals map, b, c, d)

lookupProcedure :: String -> Symbols -> [(Bool, ASTTypeDenoter)]
lookupProcedure name (map, _, _, _) = (map ! name)
