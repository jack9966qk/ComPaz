-- Compaz Stage 3
-- Functionalities for Code Generation

-- CodeGen monad adapted from lecture
type LabelCounter = Int
data State = State Env LabelCounter
data CodeGen a = CodeGen (State -> (a, State))

instance Monad CodeGen where
    return code
        = CodeGen (\st -> (code, st))
    CodeGen >>= f 
        = CodeGen (\st0 ->
            let
                (code', st1) = gen st0
                CodeGen gen' = f code'
            in gen' st1)

getState :: CodeGen State
getState = CodeGen (\st -> (st, st))

getLabelCounter :: CodeGen LabelCounter
getLabelCounter = do
    State env lc <- getState
    return lc

incLabelCounter :: CodeGen ()
incLabelCounter = CodeGen (\(State env lc) ->
    ((), State env (lc + 1)))
