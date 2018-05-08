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
    Map Reg ASTTypeDenoter
    )

initSymbols :: Symbols
initSymbols = (Map.empty, Map.empty, Map.empty)

insertRegType :: Reg -> ASTTypeDenoter -> Symbols -> Symbols
insertRegType r t (a, b, map) = -- trace (show $ insert r t map)
    (a, b, insert r t map)

lookupRegType :: Reg -> Symbols -> ASTTypeDenoter
lookupRegType r (_, _, map) = -- trace ( (show map) ++ " get " ++ (show r) )
    (map ! r)