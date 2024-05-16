module Imp.Interpreter.ImpInterpreter (interpProgram) where

import Data.Char
import Data.List (union)
import Imp.Syntax.Syntax


type Env = [(Var, Value)]

(.+.) :: Value -> Value -> Value
(EInt n) .+. (EInt m) = EInt (n + m)
_        .+. _        = error "Type error"

(.*.) :: Value -> Value -> Value
(EInt n) .*. (EInt m) = EInt (n * m)
_        .*. _        = error "Type error"

(.-.) :: Value -> Value -> Value
(EInt n) .-. (EInt m) = EInt (n - m)
_        .-. _        = error "Type error"

(./.) :: Value -> Value -> Value
(EInt n) ./. (EInt m) = EInt (div n m)
_        ./. _        = error "Type error"


(.&.) :: Value -> Value -> Value
(EBool b1) .&. (EBool b2) = EBool (b1 && b2)
_          .&. _         = error "Type error"

(.<.) :: Value -> Value -> Value
(EInt b1) .<. (EInt b2) = EBool (b1 < b2)
_         .<. _         = error "Type error"

(.=.) :: Value -> Value -> Value
(EInt b1) .=. (EInt b2) = EBool (b1 == b2)
_         .=. _         = error "Type error"


printValue :: Value -> IO ()
printValue (EInt n) = print n
printValue (EBool b) = print b

readValue :: IO Value
readValue
  = do
      s <- getLine
      if all isDigit s then return $ EInt $ read s
        else if s `elem` ["True", "False"] then return $ EBool $ read s
             else error "type error"

interpBinOp :: Env -> (Value -> Value -> Value) -> Exp -> Exp -> IO Value
interpBinOp env f e1 e2
  = do
       v1 <- interpExp env e1
       v2 <- interpExp env e2
       return (f v1 v2)

interpExp :: Env -> Exp -> IO Value
interpExp _   (EValue v) = return v
interpExp env (EVar v)
  = case lookup v env of
      Just val -> return val
      Nothing  -> error "Undefined variable"
interpExp env (e1 :+: e2)
  = do
      v1 <- interpExp env e1
      v2 <- interpExp env e2
      return (v1 .+. v2)
interpExp env (e1 :*: e2)
  = do
      v1 <- interpExp env e1
      v2 <- interpExp env e2
      return (v1 .*. v2)
interpExp env (e1 :-: e2)
  = interpBinOp env (.-.) e1 e2
interpExp env (e1 :/: e2)
  = interpBinOp env (./.) e1 e2
interpExp env (e1 :&: e2)
  = interpBinOp env (.&.) e1 e2
interpExp env (e1 :==: e2)
  = interpBinOp env (.=.) e1 e2
interpExp env (e1 :<: e2)
  = interpBinOp env (.<.) e1 e2
interpExp env (ENot e)
  = do
       v1 <- interpExp env e
       case v1 of
         (EBool b) -> return (EBool (not b))
         _         -> error "type error!"

insertVal :: Var -> Value -> Env -> Env
insertVal v val env = (v, val) : env

defaultValue :: Ty -> Value
defaultValue TInt  = EInt 0
defaultValue TBool = EBool False

removeVars :: [Var] -> Env -> Env
removeVars vs env
  = [(x,v) | (x,v) <- env, x `notElem` vs]

interpStmt :: Env -> Stmt -> IO (Env, [Var])
interpStmt env Skip = return (env, [])
interpStmt env (Def t v me)
  = case me of
      Just e -> do
        v1 <- interpExp env e
        return (insertVal v v1 env, [v])
      Nothing -> return (insertVal v (defaultValue t) env, [v])
interpStmt env (v := e)
  = do
      val <- interpExp env e
      return (insertVal v val env, [])
interpStmt env (If e1 b1 b2)
  = do
      val1 <- interpExp env e1
      case val1 of
        EBool True ->
          do
            (env', _) <- interpBlock env b1
            return (env', [])
        EBool False ->
          do
            (env', _) <- interpBlock env b2
            return (env', [])
        _           -> error "type error"
interpStmt env (While e b)
  = do
       v1 <- interpExp env e
       case v1 of
         EBool False -> return (env, [])
         EBool True -> do
           (env1, _) <- interpBlock env b
           (env', _) <- interpStmt env1 (While e b)
           return (env', [])
         _ -> error "type error"
interpStmt env (Print e)
  = do
      v1 <- interpExp env e
      printValue v1
      return (env , [])
interpStmt env (SRead x)
  = do
      n <- readValue
      return (insertVal x n env, [])

interpBlock :: Env -> Block -> IO (Env, [Var])
interpBlock env (Block stms)
  = do
       (env1, vs) <- interpList env stms
       return (removeVars vs env1, [])

interpList :: Env -> [Stmt] -> IO (Env, [Var])
interpList env [] = return (env, [])
interpList env (s : ss)
  = do
       (env1, vs) <- interpStmt env s
       (env2, vss) <- interpList env1 ss
       return (env2, vs `union` vss)

interpProgram :: Program -> IO Env
interpProgram (Program blk)
  = fst <$> interpBlock [] blk
