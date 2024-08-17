import Mathlib.Data.Finmap
import Mathlib.Data.Nat.Defs 

import Imp.Semantics

-- definition of instructions 

inductive instr : Type where
| loadi : ℕ → instr 
| load : String → instr 
| add : instr 
| store : String → instr 
| jmp : ℕ → instr 
| jmpless : ℕ → instr 
| jmpge : ℕ → instr 
deriving Repr

-- semantics of the machine 

abbrev Stack : Type := List Value 
structure Conf : Type where 
  pc : ℕ 
  memory : Finmap (λ _ : String => Value)
  stack : Stack 

-- inductive Exec : Conf → instr → Conf → Prop where 
-- | ILoadi : ∀ n pc 

