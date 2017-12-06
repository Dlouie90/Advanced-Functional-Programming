module Tokenizer (tokenize) where

import DataDeclrs (Elmt(..), Optr(Optr), evalExpr) 

import Data.Map as M (fromList, lookup)


---------------------------------------------------------------------
-- These functions convert a string into a list of Elmts.
-- The main function is tokenize.
---------------------------------------------------------------------

-- <Your comment>
addSpaces :: String  -> String
addSpaces = foldr (\c str -> if c `elem` " 0123456789" 
                             then c:str 
                             else [' ', c, ' '] ++ str) 
                  []


-- <Your comment>
-- < What does makeToken assume about the input String? >
-- < The symbol for division is '/', but the operation performed is `div`.
--   How can you tell that from this table? >
makeToken :: String -> Elmt
makeToken str = 
  case M.lookup str $ fromList [ ( "(", LPar),             ( ")", RPar) 
                               , ( "+", Op (+) '+' 1 'L'), ( "-", Op (-) '-' 1 'L') 
                               , ( "*", Op (*) '*' 2 'L'), ( "/", Op div '/' 2 'L') 
                               , ( "^", Op (^) '^' 3 'R')
                               ] of
  Just op -> op
  Nothing -> Nbr (read str) -- How do we know we should perform: read str? 
                            -- How do we know what type  read  returns? 


-- <Your comment>
tokenize :: String -> [Elmt]
tokenize string =  
  concat [[LPar], map makeToken . words . addSpaces $ string, [RPar]]

