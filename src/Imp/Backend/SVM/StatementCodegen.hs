module Imp.Backend.SVM.StatementCodegen (compileProgram) where

import Imp.Syntax.Syntax
import Imp.Backend.SVM.ExpCodegen
import SVM.Instr

compileProgram :: Program -> IO Code
compileProgram (Program blk)
  =  (++ [HALT]) <$> runCompileM (compileBlock blk)

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
          elsesize = length celse
      return $ concat [ ce, [JZ $ thensize + 1]
                      , cthen, [JMP $ elsesize + 1]
                      , celse]
compileStatement (While e blk)
  = do
      ce <- compileExpr e
      cblock <- compileBlock blk
      let blksize = length cblock
          expsize = length ce
          back = - (blksize + expsize + 1)
      return $ concat [ ce, [JZ (blksize + 2)], cblock
                      , [JMP back]
                      ]

compileInit :: Maybe Exp -> CompileM Code
compileInit Nothing = return [PUSHI 0]
compileInit (Just e) = compileExpr e

compileBlock :: Block -> CompileM Code
compileBlock (Block blk)
  = concat <$> mapM compileStatement blk
