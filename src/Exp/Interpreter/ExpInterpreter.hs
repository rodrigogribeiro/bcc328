module Exp.Interpreter.ExpInterpreter where 

import Exp.Syntax.Syntax 

expInterp :: Exp -> Int 
expInterp (EInt n) = n 
expInterp (e1 :+: e2) 
  = expInterp e1 + expInterp e2
expInterp (e1 :*: e2)
  = expInterp e1 * expInterp e2
