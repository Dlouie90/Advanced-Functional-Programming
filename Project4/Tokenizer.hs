module Tokenizer (tokenize) where

import DataDeclrs (Elmt(..), Optr(Optr), evalExpr) 

import Data.Map as M (fromList, lookup)


---------------------------------------------------------------------
-- These functions convert a string into a list of Elmts.
-- The main function is tokenize.
---------------------------------------------------------------------

-- addSpaces checks if c (last element in the string) is an number and then adds it to the head of the string. If
-- it is not a number, then concat the str to a list with space elements in between them.
addSpaces :: String  -> String
addSpaces = foldr (\c str -> if c `elem` " 0123456789" 
                             then c:str 
                             else [' ', c, ' '] ++ str) 
                  []


-- makeToken takes in a str which is one of the symbols and returns information about
-- the operator.
-- < What does makeToken assume about the input String? >
-- It assumes the string is either a number or a mathematical symbol.
-- < The symbol for division is '/', but the operation performed is `div`.
--   How can you tell that from this table? >
-- The sixth tuple shows that the "/" symbol is the div operator because Op means
-- operator and it says Op div.
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


-- tokenize creates a list of Elmt out of the string you put in and making use of 
-- all the functions above and the words function.
tokenize :: String -> [Elmt]
tokenize string =  
  concat [[LPar], map makeToken . words . addSpaces $ string, [RPar]]

