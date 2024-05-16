-- |

module Imp.Syntax.Syntax where

-- definition of expressions

newtype Var = Var { unVar :: String } deriving (Eq, Ord, Show)

data Value
  = EInt Int
  | EBool Bool
  deriving (Eq, Ord, Show)

data Exp
  = EValue Value
  | EVar Var
  | Exp :+: Exp
  | Exp :*: Exp
  | Exp :-: Exp
  | Exp :/: Exp
  | Exp :==: Exp
  | Exp :<: Exp
  | ENot Exp
  | Exp :&: Exp
  deriving (Eq, Ord, Show)

-- types

data Ty = TInt | TBool deriving (Eq, Ord, Show)

-- syntax for statements

newtype Program
  = Program {unProgram :: Block}
    deriving (Eq, Ord, Show)

newtype Block
  = Block { unBlock :: [Stmt] }
    deriving (Eq, Ord, Show)

data Stmt
  = Skip
  | Def Ty Var (Maybe Exp)
  | Var := Exp
  | If Exp Block Block
  | Print Exp
  | SRead Var
  | While Exp Block
  deriving (Eq, Ord, Show)
