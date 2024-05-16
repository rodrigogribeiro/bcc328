module Imp.Frontend.Semantics.StatementTypeChecker where

import Imp.Frontend.Semantics.Basics
import Imp.Frontend.Semantics.ExpTypeChecker
import Imp.Syntax.Syntax


tcProgram :: Program -> Check ()
tcProgram (Program blk)
  = case tcBlock [] blk of 
      Left err -> Left err 
      Right _  -> Right ()


tcBlock :: Ctx -> Block -> Check Ctx 
tcBlock ctx (Block stmts) 
  = tcStmts ctx stmts 

tcStmts :: Ctx -> [Stmt] -> Check Ctx 
tcStmts ctx [] = Right ctx 
tcStmts ctx (s : ss) 
  = case tcStmt ctx s of 
      Left err -> Left err 
      Right ctx' -> tcStmts ctx' ss

tcInit :: Ctx -> Maybe Exp -> Ty -> Check ()
tcInit _ Nothing _ = Right () 
tcInit ctx (Just e) ty 
  = case tcExp ctx e of
      Left err -> Left err 
      Right ty' -> if ty == ty' 
                   then Right ()
                   else incompatibleTypes ty ty' 

tcStmt :: Ctx -> Stmt -> Check Ctx 
tcStmt ctx Skip 
  = Right ctx 
tcStmt ctx (Def ty v me)
  = case tcInit ctx me ty of 
      Left err -> Left err 
      Right _ -> Right ((v, ty) : ctx)
tcStmt ctx (v := e)
  = case (lookup v ctx, tcExp ctx e) of 
      (Just ty, Right ty') -> 
        if ty == ty' 
        then Right ctx 
        else incompatibleTypes ty ty' 
      (Nothing, _) -> undefinedVar v 
      (_ , Left err) -> Left err
tcStmt ctx (If e blk1 blk2)
  = case tcExp ctx e of 
      Left err -> Left err 
      Right TInt -> incompatibleTypes TBool TInt
      Right _ -> 
        case (tcBlock ctx blk1, tcBlock ctx blk2) of 
          (Left err1, Left err2) -> Left $ err1 ++ err2 
          (Left err1, _) -> Left err1 
          (_, Left err2) -> Left err2 
          (Right _, Right _) -> Right ctx
tcStmt ctx (While e blk)
  = case tcExp ctx e of 
      Left err -> Left err 
      Right TInt -> incompatibleTypes TBool TInt 
      Right _ -> 
        case tcBlock ctx blk of 
          Left err1 -> Left err1 
          Right _   -> Right ctx 
tcStmt ctx (Print e) 
  = case tcExp ctx e of 
      Left err -> Left err 
      Right _  -> Right ctx 
tcStmt ctx (SRead v)
  = case lookup v ctx of 
      Nothing -> undefinedVar v 
      _       -> Right ctx 
