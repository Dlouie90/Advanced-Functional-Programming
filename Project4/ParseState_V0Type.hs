module ParseState_V0Type (evalArith) where

import DataDeclrs (Elmt, evalExpr) 
import Tokenizer (tokenize)
import ParseStateClass (ParseState(..), replace) 

---------------------------------------------------------------------
-- Define  ParseState_V1  to three lists, Left, Window, and Right.
-- Define how  ParseState_V1  implements  ParseState.  
---------------------------------------------------------------------

-- 
data ParseState_V0 = LeftWindowRight [Elmt] [Elmt] [Elmt] deriving Eq

---------------------------------------------------------------------
-- evalArith is the top-level function. 
-- Because it generates an intermediate ParseState, 
-- it must be defined for each ParseState type.
---------------------------------------------------------------------

-- <Your comment>
evalArith :: String -> (String, Elmt, Int) 
evalArith string = (string, expr, evalExpr expr)
  where ([], [expr], []) = toLWR (parse . tokenize $ string :: ParseState_V0)
  

-- Define how to perform the ParseState functions on ParseState_V0 objects
instance ParseState ParseState_V0 where

  -- <Your comment>
  initialParseState tokens = LeftWindowRight [] window rest
    where (window, rest) = splitAt 5 tokens


  -- <Your comment>
  left (LeftWindowRight lft _ _)     = lft
  window (LeftWindowRight _ w _)     = w
  right (LeftWindowRight _ _ r)      = r
  toParseState (left, window, right) = LeftWindowRight left window right 

