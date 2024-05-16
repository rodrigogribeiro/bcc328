module Imp.Frontend.Semantics.ExpTypeChecker where

import Imp.Syntax.Syntax
import Imp.Frontend.Semantics.Basics

-- typing values

tcValue :: Value -> Check Ty
tcValue (EInt _) = Right TInt
tcValue (EBool _) = Right TBool

-- type checking expressions

tcBinOp :: Ctx -> (Exp, Expected) -> (Exp, Expected) -> Expected -> Check Ty
tcBinOp ctx (e1, t1) (e2, t2) ty
  = case (r1, r2) of
      (Right t1', Right t2') ->
         if t1' == t1 then
            if t2' == t2 then
               Right ty
            else incompatibleTypes t2 t2'
         else incompatibleTypes t1 t1'
      (Left err1, Left err2) -> Left (err1 ++ err2)
      (Left err1, _) -> Left err1
      (_, Left err2) -> Left err2
  where
    r1 = tcExp ctx e1
    r2 = tcExp ctx e2

tcExp :: Ctx -> Exp -> Check Ty
tcExp _ (EValue v)
  = tcValue v
tcExp ctx (EVar v)
  = case lookup v ctx of
      Just ty -> Right ty
      Nothing -> undefinedVar v
tcExp ctx (e1 :+: e2)
  = tcBinOp ctx (e1, TInt) (e2, TInt) TInt
tcExp ctx (e1 :*: e2)
  = tcBinOp ctx (e1, TInt) (e2, TInt) TInt
tcExp ctx (e1 :-: e2)
  = tcBinOp ctx (e1, TInt) (e2, TInt) TInt
tcExp ctx (e1 :/: e2)
  = tcBinOp ctx (e1, TInt) (e2, TInt) TInt
tcExp ctx (e1 :==: e2)
  = tcBinOp ctx (e1, TInt) (e2, TInt) TBool
tcExp ctx (e1 :<: e2)
  = tcBinOp ctx (e1, TInt) (e2, TInt) TBool
tcExp ctx (e1 :&: e2)
  = tcBinOp ctx (e1, TBool) (e2, TBool) TBool
tcExp ctx (ENot e1)
  = case tcExp ctx e1 of
      Right TBool -> Right TBool
      Right TInt -> incompatibleTypes TBool TInt
      Left err -> Left err
