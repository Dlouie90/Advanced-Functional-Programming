module ParseStateClass (ParseState(..), replace, trim) where

import DataDeclrs (Elmt(..), Optr(Optr)) 

import Debug.Trace


-----------------------------------------------------------------------
-- The ParseState TypeClass. A TypeClass is like an Interface in Java.
-- Define a  ParseState  as supporting these functions. Also, require 
-- that every  ParseState  satisfies (i.e., implements)  Eq.
-----------------------------------------------------------------------

-- These functions will depend on the specific implementation
-- of a ParseState.
class Eq parseState => ParseState parseState where
  initialParseState :: [Elmt] -> parseState

  left :: parseState -> [Elmt]
  window :: parseState -> [Elmt]
  right :: parseState -> [Elmt]
  toParseState :: ([Elmt], [Elmt], [Elmt]) -> parseState


  -- continueParse takes in a parseState and checks if the parseState needs to
  -- continue parsing. If the pState is in the correct format with a triple tuple
  -- with empty lists on the sides and the list in the middle then it is false. If not,
  -- then returns true.
  continueParse :: parseState -> Bool
  continueParse pState 
    | ([], [_], []) <- toLWR pState = False
    | otherwise                     = True


  -- The following functions are implementation-independent 
  -- and can be used by all  ParseState  instances.
  -- parse takes in a list of Elmt and returns the current parseState.
  parse :: [Elmt] -> parseState  
  parse tokens = 
    perhapsTrace (" Start " ++ showParseState initialPState) $
                 while initialPState continueParse parseAStep
    where initialPState = initialParseState tokens


  -- <Your comment>
  parseAStep :: parseState -> parseState
  parseAStep pState = 
    perhapsTrace ("   ==> "  ++ showParseState newState) 
                 newState    
    where newState = 
              case toLWR pState of
              (left, [], [])        -> toParseState $ ([], [Error left], [])
              (left, window, right) -> 
                let updatedWindow = applyARule window 
                in slideWindow (if updatedWindow /= window then (-3) else 1) 
                               $ toParseState (left, updatedWindow, right)
                    

  -- <Your comment>
  showParseState :: parseState -> String
  showParseState pState = 
    let (left, window, right) = toLWR pState   
    in                (trim . show) left 
       ++ "   << " ++ (replace "," ", " . trim . show) window ++ " >>   " 
       ++             (trim . show) right


  slideWindow :: Int -> parseState -> parseState
  slideWindow n pState
    | n < 0  = -- Since n < 0, length left + n < length n
        let (left', shifted)  = splitAt (length left + n) left 
            (window', right') = splitAt 5 (shifted ++ window ++ right)
        in toParseState (left', window', right')  
    | n == 0 = pState
    | n > 0  = 
        let (shifted, windRight) = splitAt n (window ++ right)
            (window', right')    = splitAt 5 windRight
        in toParseState (left ++ shifted, window', right') 
    where (left, window, right) = toLWR pState                             



  -- <Your comment>
  toLWR :: parseState -> ([Elmt], [Elmt], [Elmt])
  toLWR pState = (left pState, window pState, right pState)  


-- --------------------------------------------------------------------
-- -- Utility functions used by ParseState functions.
-- --------------------------------------------------------------------


-- <Your comment>
applyARule :: [Elmt] -> [Elmt]
-- <Your comment. What does this rule do?>
applyARule window
  | ([LPar, e, RPar], rest) <- splitAt 3 window
  , isOperand e                = [e] ++ rest
-- <Your comment. What does this rule do?>
applyARule [op1, e1, op@(Op opfn c _ _), e2, op2]
    |  isOperand e1 && isOperand e2
    && higherPrec op1 op op2 = [op1, Expr e1 (Optr opfn c) e2, op2]
-- <Your comment. Why is this defined?>
applyARule window = window


-- <Your comment>
higherPrec :: Elmt -> Elmt -> Elmt -> Bool  
higherPrec leftOp op rightOp =
  higherPrecThanLeft leftOp op && higherPrecThanRight op rightOp  


-- <Your comment>
higherPrecThanLeft :: Elmt -> Elmt -> Bool
higherPrecThanLeft LPar          _  = True
higherPrecThanLeft (Op _ _ p0 _) (Op _ _ p1 _) = p0 < p1
higherPrecThanLeft _             _             = False
  

-- <Your comment>
higherPrecThanRight :: Elmt -> Elmt -> Bool
higherPrecThanRight _             RPar          = True
higherPrecThanRight (Op _ _ p1 _) (Op _ _ p2 _) = p1 >= p2
higherPrecThanRight _             _             = False


isOperand :: Elmt -> Bool
isOperand (Nbr _)      = True
isOperand (Expr _ _ _) = True
isOperand _            = False


-- <Your comment>
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace xs ys zs 
  | (start, rest) <- splitAt (length xs) zs
  , start == xs = ys ++ replace xs ys rest
  | w:ws <- zs  = w : replace xs ys ws
  | null zs     = []


-- slideWindow' :: Int -> ([Elmt], [Elmt], [Elmt]) -> ([Elmt], [Elmt], [Elmt])
-- slideWindow' n lwr@(left, window, right)
--   | n < 0  = -- Since n < 0, length left + n < length n
--       let (left', shifted)  = splitAt (length left + n) left 
--           (window', right') = splitAt 5 (shifted ++ window ++ right)
--       in (left', window', right')  
--   | n == 0 = lwr
--   | n > 0  = 
--       let (shifted, windRight) = splitAt n (window ++ right)
--           (window', right')    = splitAt 5 windRight
--       in (left ++ shifted, window', right') 


-- trim takes in a string and checks if the first character in the list is 
-- a bracket or par and if it is true, it returns the string without them.
-- If they don't have any containers, then return the string.  
trim :: String -> String
trim (x:rest) | x `elem` "([" = init rest
trim str        = str


-- while is similar to project 3 where is checks if it should continue
--  
while :: state -> (state -> Bool) -> (state -> state) -> state
while state continueTest bodyFn = wh state
  -- The auxiliary function is not necessary. Defining it just
  -- avoids repeatedly passing  continueTest  and  bodyFn.
  where 
    wh st
      | continueTest st = wh (bodyFn st)
      | otherwise       = st 


-- ---------------------------------------------------------
--  Trace stuff
-- ---------------------------------------------------------

perhapsTrace :: String -> a ->  a
perhapsTrace str e = if   shouldTrace 
                     then trace str e
                     else e
  where shouldTrace = False