module Exp.Backend.C.ExpCCodegen where 

import Exp.Syntax.Syntax 


-- top level code generation function 

cExpCodegen :: Exp -> String 
cExpCodegen e 
  = unlines [ "#include <stdio.h>"
            , "// code generated for expressions"
            , "int main () {"
            , nest 3 $ generateBody e 
            , nest 3 "return 0;"
            , "}"
            ] 
    where
      nest n v = replicate n ' ' <> v

generateBody :: Exp -> String 
generateBody e 
  = unlines [ generateAssignment e 
            , generatePrint 
            ]


generateAssignment :: Exp -> String 
generateAssignment e 
  = unwords [ "int val ="
            , pprintExp e 
            , ";" 
            ]

generatePrint :: String 
generatePrint = "printf(\"%d\", val);"
