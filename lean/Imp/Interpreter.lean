import Mathlib.Data.Nat.Defs 
import Mathlib.Tactic.Basic 

import Imp.Syntax

-- environments 

abbrev Value := ℕ 

inductive Result (A : Type) : Type where 
| Ok : A → Result A 
| TypeError : Result A 
| OutOfFuel : Result A  

-- semantics of expressions 

abbrev Env := String → Value 

def emptyEnv : Env := λ _ => 0

def updateEnv : String → Value → Env → Env
| s, v, env => λ s' =>
  if Substring.beq s.toSubstring s'.toSubstring
  then v
  else env s

macro  x:term "|->" v:term ";" s:term  : term => 
  `(updateEnv $x $v $s)

def vlt (n1 n2 : Value) : Value := 
  if Nat.blt n1 n2 then 1 else 0

def vand (n1 n2 : Value) : Value := 
  if Nat.beq n1 0 then 0 else n2 

def vnot (n1 : Value) : Value := 
  if Nat.ble n1 0 then 1 else 0  

def evalBOp : Binop → Value → Value → Value
| Binop.BPlus => λ x y => x + y  
| Binop.BLess => vlt 
| Binop.BAnd => vand  

def evalExp : IExp → Env → Value
| IExp.ELit (Lit.INat l), _ => l 
| IExp.EVar v, env => env v
| IExp.EUn _ e1, env => vnot (evalExp e1 env)
| IExp.BOp op e1 e2, env => 
  (evalBOp op) (evalExp e1 env) (evalExp e2 env)

abbrev Fuel := ℕ

def evalStmt : Fuel → Stmt → Env → Result Env 
| 0 , _ , _ => Result.OutOfFuel
| _ + 1, Stmt.Skip , env => Result.Ok env 
| _ + 1, Stmt.Assign v e, env =>
  Result.Ok (v |-> (evalExp e env) ; env)
| fuel' + 1, Stmt.Seq s1 s2, env => 
  match evalStmt fuel' s1 env with 
  | Result.Ok env1 => evalStmt fuel' s2 env1
  | r => r 
| fuel' + 1, Stmt.If e s1 s2, env => 
  match evalExp e env with 
  | 0 => 
    evalStmt fuel' s2 env 
  | _ + 1 => 
    evalStmt fuel' s1 env 
| fuel' + 1, Stmt.While e s1, env => 
  match evalExp e env with 
  | _ + 1 => 
    match evalStmt fuel' s1 env with 
    | Result.Ok env1 => 
      evalStmt fuel' (Stmt.While e s1) env1
    | r => r 
  | 0 => Result.Ok env 

