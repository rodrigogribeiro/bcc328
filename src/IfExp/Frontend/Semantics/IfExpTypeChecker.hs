module IfExp.Frontend.Semantics.IfExpTypeChecker where 

import IfExp.Syntax.Syntax 


data Check a 
  = Ok a 
  | TypeMismatch Expected Found IExp  

-- definindo tipos 

data Ty = Nat | Bool 
  deriving (Eq, Show)

type Expected = Ty 
type Found = Ty 

typeCheck :: IExp -> Check Ty 
typeCheck IZero = Ok Nat 
typeCheck ITrue = Ok Bool 
typeCheck IFalse = Ok Bool 
typeCheck (ISucc e)
  = case typeCheck e of 
      Ok Nat -> Ok Nat 
      Ok Bool -> 
        TypeMismatch Nat Bool (ISucc e)
      err -> err 
typeCheck (IPred e)
  = case typeCheck e of 
      Ok Nat -> Ok Nat 
      Ok Bool -> 
        TypeMismatch Nat Bool (IPred e)
      err -> err 
typeCheck (IIsZero e)
  = case typeCheck e of 
      Ok Nat -> Ok Bool
      Ok Bool -> 
        TypeMismatch Nat Bool (IsZero e)
      err -> err 
typeCheck e@(IIf e1 e2 e3) 
  = case typeCheck e1 of 
      Ok Bool -> 
        case (typeCheck e2, typeCheck e3) of 
          (Ok t1, Ok t2) -> 
            if t1 == t2 then Ok t1
              else TypeMismatch t1 t2 e 
          (err, Ok _) -> err 
          (Ok _, err) -> err 
      Ok Nat -> 
        TypeMismatch Bool Nat e1
      err -> err 
