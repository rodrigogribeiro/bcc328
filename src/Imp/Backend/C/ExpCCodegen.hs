module Imp.Backend.C.ExpCCodegen where

import Imp.Syntax.Syntax


-- top level code generation for expressions. 
-- code uses the grammar structure for printing 
-- code using the right precedence.

expCodeGen :: Exp -> String 
expCodeGen (e1 :&: e2)
  = d1 <> " && " <> d2 
  where 
      d1 = expCodeGen e1 
      d2 = expCodeGen e2 
expCodeGen (e1 :==: e2)
  = d1 <> " == " <> d2 
    where 
      d1 = expCodeGen e1 
      d2 = expCodeGen e2 
expCodeGen (e1 :<: e2)
  = d1 <> " < " <> d2 
    where 
      d1 = expCodeGen e1 
      d2 = expCodeGen e2 
expCodeGen other 
  = sumCodeGen other 

-- sum level of precedence

sumCodeGen :: Exp -> String 
sumCodeGen (e1 :+: e2)
  = d1 <> " + " <> d2 
    where 
      d1 = sumCodeGen e1 
      d2 = sumCodeGen e2 
sumCodeGen (e1 :-: e2)
  = d1 <> " - " <> d2 
    where 
      d1 = sumCodeGen e1 
      d2 = sumCodeGen e2
sumCodeGen other = termCodeGen other 

-- product level (terms)

termCodeGen :: Exp -> String 
termCodeGen (e1 :*: e2)
  = d1 <> " * " <> d2 
    where 
      d1 = termCodeGen e1 
      d2 = termCodeGen e2  
termCodeGen (e1 :/: e2)
  = d1 <> " / " <> d2 
    where 
      d1 = termCodeGen e1 
      d2 = termCodeGen e2  
termCodeGen other
  = factorCodeGen other 

-- factors

factorCodeGen :: Exp -> String
factorCodeGen (EValue v)
  = valueCodeGen v 
factorCodeGen (EVar v)
  = varCodeGen v 
factorCodeGen other 
  = '(' : expCodeGen other ++ ")"

varCodeGen :: Var -> String
varCodeGen (Var v) 
  = v

valueCodeGen :: Value -> String 
valueCodeGen (EInt n) = show n 
valueCodeGen (EBool b)
  = if b then "1" else "0"


