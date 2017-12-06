module ParseState_V2Type (evalArith) where

import DataDeclrs (Elmt, evalExpr)
import Tokenizer (tokenize)
import ParseStateClass (ParseState(..), replace)

import Debug.Trace


---------------------------------------------------------------------
-- Define  ParseState_V2  to be an [Elmt] list with an index indicating the
-- start of the window. Define how  ParseState_V2  implements  ParseState.  
---------------------------------------------------------------------

-- <Your comment>
data ParseState_V2 = IndexAndList Int [Elmt] deriving Eq

---------------------------------------------------------------------
-- evalArith is the top-level function. 
-- Because it generates an intermediate ParseState, 
-- it must be defined for each ParseState type.
---------------------------------------------------------------------

-- <Your comment>
evalArith :: String -> (String, Elmt, Int) 
evalArith string = (string, expr, evalExpr expr)
  where ([], [expr], []) = toLWR (parse . tokenize $ string :: ParseState_V2)


-- Define how to perform the ParseState functions on ParseState_V2 objects
instance ParseState ParseState_V2 where

  -- <Your comment>
  initialParseState tokens = IndexAndList 0 tokens


  -- <Your comment>
  left (IndexAndList index list)       = take index list
  window (IndexAndList index list)     = take 5 $ drop index list
  right (IndexAndList index list)      = drop (index+5) list
  toParseState (left, window, right)   = IndexAndList (length left) (left ++ window ++ right)


  slideWindow n (IndexAndList i lst) = 
    IndexAndList (max 0 (min (length lst) (i + n))) lst 

