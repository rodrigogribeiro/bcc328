module IfExp.Syntax.Syntax where 

data IExp 
  = IZero 
  | ISucc IExp 
  | IIsZero IExp 
  | IPred IExp 
  | ITrue 
  | IFalse 
  | IIf IExp IExp IExp 
  deriving (Eq, Show)


