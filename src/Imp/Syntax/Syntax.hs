-- |

module Imp.Syntax.Syntax where

import Data.List

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

class Vars a where 
  vars :: a -> [Var]

instance Vars Stmt where 
  vars (Def _ v _) = [v]
  vars (If _ bt be) 
    = vars bt ++ vars be
  vars (While _ bw)
    = vars bw 
  vars _ = []

instance Vars Block where 
  vars (Block blk) 
    = nub $ concatMap vars blk
