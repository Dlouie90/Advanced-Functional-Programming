module DataDeclrs 
( Elmt(..)  -- Export all the  Elmt  constructors.
, Optr (Optr)  
, evalExpr
) where 

---------------------------------------------------------------------
-- Data declarations.
---------------------------------------------------------------------

-- These are elements in the sequence to be parsed. Also, Nbr and 
-- Expr are components of the eventual expression produced by the parse.
data Elmt =   Nbr Int 
            | LPar
            | RPar
            | Op (Int -> Int -> Int) Char Int Char
            | Expr Elmt Optr Elmt
            | Error [Elmt] 

-- Optr  is the operator component of an  Expr  node. 
-- Using it makes it possible to require that the middle
-- Expr  component be an operator rather than just
-- another  Elmt, which happens to be an Op.           
data Optr = Optr (Int -> Int -> Int) Char


-- <Your comment>
evalExpr :: Elmt -> Int
evalExpr (Nbr n) = n
evalExpr (Expr e1 (Optr op _) e2) =  (evalExpr e1) `op` (evalExpr e2) 
evalExpr (Error elmts) = -1 -- Would prefer NaN, but it's not an Int.


-- <Your comment>
instance Show Elmt where  
    show LPar           = "("
    show RPar           = ")"
    show (Nbr n)        = show n
    show (Op _ c _ _)   = [c] -- Assume every operator is one character.
    show (Expr e1 (Optr _ c) e2) 
                        = concat["(", show e1, [c], show e2, ")"] 
    show (Error elmts)  = "Error:" ++ tail (concat $ map ((", " ++) . show) elmts)

-- <Your comment>
instance Eq Elmt where
    Nbr n1       == Nbr n2       = n1 == n2
    LPar         == LPar         = True  
    RPar         == RPar         = True  
    Op _ c1 _ _  == Op _ c2 _ _  = c1 == c2
    Expr e11 optr1 e12 
                 == Expr e21 optr2 e22
                                 = e11 == e21 && optr1 == optr2 && e12 == e22 
    Error elmts1 == Error elmts2 = elmts1 == elmts2 
    _            == _            = False       

-- <Your comment>
instance Eq Optr where
    (Optr _ c1) == (Optr _ c2) = c1 == c2                     


