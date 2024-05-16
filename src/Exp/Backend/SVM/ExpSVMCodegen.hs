module Exp.Backend.SVM.ExpSVMCodegen where


import Exp.Syntax.Syntax 
import SVM.Instr 

svmExpCodegen :: Exp -> [Instr]
svmExpCodegen e 
  = svmExpCodegen' e ++ [OUT, HALT]
    where 
      svmExpCodegen' (EInt n) = [PUSHI n]
      svmExpCodegen' (e1 :+: e2)
        = svmExpCodegen' e1 ++ 
          svmExpCodegen' e2 ++ [ADD]
      svmExpCodegen' (e1 :*: e2)
        = svmExpCodegen' e1 ++ 
          svmExpCodegen' e2 ++ [MUL]
