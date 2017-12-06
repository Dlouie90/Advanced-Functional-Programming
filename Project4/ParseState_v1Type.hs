module ParseState_V1Type (evalArith) where

import DataDeclrs (Elmt, evalExpr)
import Tokenizer (tokenize)
import ParseStateClass (ParseState(..), replace) 

---------------------------------------------------------------------
-- Define  ParseState_V1  to be a PairOfLists of [Elmt] lists.
-- Define how  ParseState_V1  implements  ParseState.  
---------------------------------------------------------------------

-- <Your comment>
data ParseState_V1 = PairOfLists [Elmt] [Elmt] deriving Eq

---------------------------------------------------------------------
-- evalArith is the top-level function. 
-- Because it generates an intermediate ParseState, 
-- it must be defined for each ParseState type.
---------------------------------------------------------------------

-- <Your comment>
evalArith :: String -> (String, Elmt, Int) 
evalArith string = (string, expr, evalExpr expr)
  where ([], [expr], []) = toLWR (parse . tokenize $ string :: ParseState_V1)
  

-- Define how to perform the ParseState functions on ParseState_V1 objects
instance ParseState ParseState_V1 where

  -- <Your comment>
  initialParseState tokens = PairOfLists [] tokens



  -- <Your comment>
  left (PairOfLists lft _)           = reverse lft
  window (PairOfLists _ rght)        = take 5 rght
  right (PairOfLists _ rght)         = drop 5 rght
  toParseState (left, window, right) = PairOfLists (reverse left) (window ++ right)

