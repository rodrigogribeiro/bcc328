import Mathlib.Data.Nat.Defs 
import Mathlib.Tactic.Basic 

import Imp.Syntax
import Imp.Interpreter 

-- semantics of statements 

inductive Eval : Env → Stmt → Env → Prop
| ESkip : ∀ env, Eval env Stmt.Skip env
| EAssign : ∀ env s e v,
            evalExp e env = v →
            Eval env (Stmt.Assign s e) (s |-> v ; env)
| ESeq : ∀ env env1 env2 s1 s2,
           Eval env s1 env1 →
           Eval env1 s2 env2 →
           Eval env (Stmt.Seq s1 s2) env2
| EIfTrue : ∀ env e s1 s2 env1 n,
            evalExp e env = n + 1 →
            Eval env s1 env1 →
            Eval env (Stmt.If e s1 s2) env1
| EIfFalse : ∀ env e s1 s2 env2,
            evalExp e env = 0 →
            Eval env s2 env2 →
            Eval env (Stmt.If e s1 s2) env2
| EWhileFalse : ∀ env e s1,
            evalExp e env = 0 →
            Eval env (Stmt.While e s1) env
| EWhileTrue : ∀ env env1 env2 e s1 n,
            evalExp e env = n + 1 →
            Eval env s1 env1 →
            Eval env1 (Stmt.While e s1) env2 →
            Eval env (Stmt.While e s1) env2


macro "<*" env:term "|" s: term "*>" "==>" env1:term : term =>
  `(Eval $env $s $env1)
