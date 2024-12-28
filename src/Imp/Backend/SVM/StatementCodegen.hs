module Imp.Backend.SVM.StatementCodegen (compileProgram) where

import Imp.Syntax.Syntax
import Imp.Backend.SVM.ExpCodegen
import SVM.Instr

compileProgram :: Program -> IO Code
compileProgram (Program blk)
  = runCompileM $ do 
      code <- compileBlock blk
      bfree <- freeVars (vars blk)
      pure (code ++ bfree ++ [HALT])

compileStatement :: Stmt -> CompileM Code
compileStatement Skip = return []
compileStatement (Def _ v einit)
  = do
      addr <- lookupVar v
      c1 <- compileInit einit
      return $ PUSHI addr : c1 ++ [STORE]
compileStatement (v := e)
  = do
      addr <- lookupVar v
      c1 <- compileExpr e
      return $ PUSHI addr : c1 ++ [STORE]
compileStatement (Print e)
  = do
      c <- compileExpr e
      return (c ++ [OUT])
compileStatement (SRead v)
  = do
      addr <- lookupVar v
      return [PUSHI addr, IN, STORE]
compileStatement (If e bthen belse)
  = do
      ce <- compileExpr e
      cthen <- compileBlock bthen
      celse <- compileBlock belse
      let thensize = length cthen
          thenvars = vars bthen 
          elsesize = length celse
          elsevars = vars belse
      fthen <- freeVars thenvars
      felse <- freeVars elsevars
      return $ concat [ ce, [JZ $ thensize + 1]
                      , cthen
                      , fthen, [JMP $ elsesize + 1]
                      , celse, felse ]
compileStatement (While e blk)
  = do
      ce <- compileExpr e
      cblock <- compileBlock blk
      let blksize = length cblock
          expsize = length ce
          back = - (blksize + expsize + 1)
      bfree <- freeVars (vars blk)
      return $ concat [ ce, [JZ (blksize + 2)], cblock
                      , [JMP back]
                      , bfree 
                      ]

freeVars :: [Var] -> CompileM Code 
freeVars 
  = mapM freeVar 

freeVar :: Var -> CompileM Instr 
freeVar v 
  = do 
      addr <- lookupVar v 
      pure (FREE addr)

compileInit :: Maybe Exp -> CompileM Code
compileInit Nothing = return [PUSHI 0]
compileInit (Just e) = compileExpr e

compileBlock :: Block -> CompileM Code
compileBlock (Block blk)
  = concat <$> mapM compileStatement blk
