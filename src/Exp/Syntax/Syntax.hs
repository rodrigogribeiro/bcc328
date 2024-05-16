module Exp.Syntax.Syntax where 

data Exp 
  = EInt Int 
  | Exp :+: Exp 
  | Exp :*: Exp 
  deriving (Eq, Ord, Show)

pprintExp :: Exp -> String 
pprintExp (e1 :+: e2)
  = unwords [ pprintExp e1
            , "+"
            , pprintExp e2]
pprintExp other 
  = pprintTerm other 


pprintTerm :: Exp -> String 
pprintTerm (e1 :*: e2)
  = unwords [ pprintTerm e1
            , "*"
            , pprintTerm e2
            ]
pprintTerm other 
  = pprintFactor other 

pprintFactor :: Exp -> String 
pprintFactor (EInt n)
  = show n
pprintFactor other 
  = unwords [ "("
            , pprintFactor other 
            , ")"]
