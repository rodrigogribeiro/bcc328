module Imp.Backend.C.StatementCCodegen where

import Imp.Syntax.Syntax
import Imp.Backend.C.ExpCCodegen


-- identation level

type Level = Int

-- top level code generation function 

programCodegen :: Program -> String  
programCodegen (Program (Block blk))
  = header <> blks
    where 
      header = unlines [ "#include <stdio.h>"
                       , "// body of IMP program"
                       , "int main() {"
                       ]
      ret = nest 3 "return 0;"
      stmts = map (stmtCodegen 3) blk ++ [ret, "}"] 
      blks = unlines stmts

-- code generation for types 

tyCodegen :: Ty -> String 
tyCodegen _ = "int"

-- code generation for variable initialization

initCodegen :: Maybe Exp -> String
initCodegen Nothing = "" 
initCodegen (Just e) = " = " ++ expCodeGen e

-- code generation for block

blockCodegen :: Level -> Block -> String
blockCodegen d (Block stmts) 
  = "{\n" <> ms <> nest (d - 3) "}" 
    where 
      ms = unlines (map (stmtCodegen d) stmts)

-- code generation for statements 

stmtCodegen :: Level -> Stmt -> String 
stmtCodegen d Skip 
  = nest d "skip;"
stmtCodegen d (Def ty v me)
  = nest d $ unwords [ tyCodegen ty
                     , varCodeGen v
                     , initCodegen me 
                     , semi 
                     ]
stmtCodegen d (v := e)
  = nest d $ unwords [ varCodeGen v 
                     , "=" 
                     , expCodeGen e
                     , semi 
                     ]
stmtCodegen d (Print e) 
  = nest d $ concat [ "printf("
                    , "\"%d\","
                    , expCodeGen e
                    , ")"
                    , semi 
                    ]
stmtCodegen d (SRead v)
  = nest d $ concat [ "scanf("
                    , "\"%d\",&"
                    , varCodeGen v
                    , ")"
                    , semi 
                    ]
stmtCodegen d (If e blk1 blk2)
  = nest d $ unwords [ "if ("
                     , expCodeGen e
                     , ")"
                     , blockCodegen (d + 3) blk1 
                     , "else"
                     , blockCodegen (d + 3) blk2
                     ]
stmtCodegen d (While e blk)
  = nest d $ unwords [ "while ("
                     , expCodeGen e 
                     , ")"
                     , blockCodegen (d + 3) blk
                     ]

-- nesting function 

nest :: Int -> String -> String
nest d s = replicate d ' ' ++ s

semi :: String 
semi = ";"
