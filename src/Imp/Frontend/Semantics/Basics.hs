module Imp.Frontend.Semantics.Basics where

import Imp.Syntax.Syntax

-- typing context

type Ctx = [(Var, Ty)]

-- type of errors

type Expected = Ty
type Found = Ty

data Error
  = IncompatibleTypes Expected Found
  | UndefinedVariable Var

pType :: Ty -> String 
pType TInt = "int"
pType TBool = "bool"

instance Show Error where 
  show (IncompatibleTypes ex fd)
    = unlines [ "Type error:"
              , "Expected:" ++ pType ex 
              , "Found:" ++ pType fd]
  show (UndefinedVariable v)
    = "Undefined variable: " ++ unVar v

undefinedVar :: Var -> Check a
undefinedVar = Left . wrap . UndefinedVariable
  where
    wrap x = [x]

incompatibleTypes :: Expected -> Found -> Check a
incompatibleTypes e f
  = Left [IncompatibleTypes e f]

-- type of type checking functions

type Check a = Either [Error] a
