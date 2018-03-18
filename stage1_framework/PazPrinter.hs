module PazPrinter where

import PazParser (
    ASTProgram
    )


astPrettyShow :: ASTProgram -> String
astPrettyShow (id, var, pro, com) =
    "ID: " ++ (show id) ++ "\n" ++
    "Var: " ++ (show var) ++ "\n" ++
    "Pro: " ++ (show pro) ++ "\n" ++
    "Com: " ++ (show com)