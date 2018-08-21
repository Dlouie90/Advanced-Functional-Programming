module ArithExprTests where

import DataDeclrs (Elmt(..))
import ParseStateClass (trim)
import qualified ParseState_V0Type as V0 (evalArith)
import qualified ParseState_V1Type as V1 (evalArith)
import qualified ParseState_V2Type as V2 (evalArith)

---------------------------------------------------------------------
-- ArithExpr Tests
---------------------------------------------------------------------

showResult :: (String, Elmt, Int) -> String
showResult (str, expr, val) = 
          "     " ++ show str 
       ++ " --> " ++ (trim . show) expr -- Strip the outer parentheses
       ++ " --> " ++ show val

                             -- left    right
testCases :: [String]        -- assoc   assoc
testCases = [ "8-4-2"        --   2       6
            , "(8-4)-2"      --   2       2
            , "8-(4-2)"      --   6       6
            , "8/4/2"        --   1       4
            , "(8/4)/2"      --   1       1
            , "8/(4/2)"      --   4       4
            , "2^3^2"        --  64     512
            , "(2^3)^2"      --  64      64
            , "2^(3^2)"      -- 512     512
            , "16-7-3*2^2"   --  -3      21

      -- Associativity not relevant for the rest.
            , "3*4+5^2"      --  37            
            , "6-3*2"        --   0       
            , "6*3-2"        --  16      
            , "6*(3-1)"      --  12      
            , "6-3*(1-7)"    --  24      
            , "(6-3)*2"      --   6       
            , "(6-3)^2+5"    --  14      
            , "5+(6-3)^2+5"  --  19      
            , "3+4/(6-4)^2"  --   4        
            , "3+4/6-4^2"    -- -13      
            , "2^(6-3)"      --   8       
            , "2^6-3"        --  61      
            , "(6-3)^2"      --   9       
            , "16-3^2"       --   7       
            , "3"            --   3 
            , "3+4/6-+4*5"   -- This example does not parse. Instead it
                             -- produces an error message showing how far  
                             -- it got: [(, (3+(4/6)), -, +, (4*5), )]   
            , "3+4/6-4*5"    -- -17
            ]    


testArithExprs :: String -> IO ()
testArithExprs version = 
  let strings = map (showResult .
                     case version of
                     "V0" -> V0.evalArith 
                     "V1" -> V1.evalArith 
                     "V2" -> V2.evalArith
                    ) 
                    testCases
  in putStr . unlines $ "   Input string --> Expr --> Value" : strings
           
