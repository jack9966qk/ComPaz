-- Compaz Stage 3
-- Functionalities for Symbol Table

module Symbol where

import Data.Map (Map)
import qualified Data.Map as Map
import PazParser (
    ASTTypeDenoter
    )

-- Adapted from Compiller.hs in provided stage 1 solution
type Symbols = (
    -- for each procedure, for each formal parameter, its varness and type
    Map String [(Bool, ASTTypeDenoter)],
    -- for each variable, its varness, type, and starting slot number
    Map String (Bool, ASTTypeDenoter, Int)
    )

initSymbols :: Symbols
initSymbols = (Map.empty, Map.empty)