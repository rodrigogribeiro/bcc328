module IfExp.Frontend.Semantics.IfExpTypeChecker where 

import IfExp.Syntax.Syntax 


data Check a 
  = Checked a 
  | TypeMismatch Expected Found IExp  

typeError :: Expected -> Found -> IExp -> String 
typeError ex fd e 
  = unlines ["Type error at:"
            , show e 
            , "Expected"
            , show ex 
            , ", but found:"
            , show fd 
            ]

-- definindo tipos 

data Ty = Nat | Bool 
  deriving (Eq, Show)

type Expected = Ty 
type Found = Ty 

typeCheck :: IExp -> Check Ty 
typeCheck IZero = Checked Nat 
typeCheck ITrue = Checked Bool 
typeCheck IFalse = Checked Bool 
typeCheck (ISucc e)
  = case typeCheck e of 
      Checked Nat -> Checked Nat 
      Checked Bool -> 
        TypeMismatch Nat Bool (ISucc e)
      err -> err 
typeCheck (IPred e)
  = case typeCheck e of 
      Checked Nat -> Checked Nat 
      Checked Bool -> 
        TypeMismatch Nat Bool (IPred e)
      err -> err 
typeCheck (IIsZero e)
  = case typeCheck e of 
      Checked Nat -> Checked Bool
      Checked Bool -> 
        TypeMismatch Nat Bool (IIsZero e)
      err -> err 
typeCheck e@(IIf e1 e2 e3) 
  = case typeCheck e1 of 
      Checked Bool -> 
        case (typeCheck e2, typeCheck e3) of 
          (Checked t1, Checked t2) -> 
            if t1 == t2 then Checked t1
              else TypeMismatch t1 t2 e 
          (err, Checked _) -> err 
          (Checked _, err) -> err
          (err, _) -> err 
      Checked Nat -> 
        TypeMismatch Bool Nat e1
      err -> err 
