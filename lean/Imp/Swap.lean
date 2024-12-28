import Imp.Syntax 
import Imp.Hoare 
import Imp.Derived

def swap : Stmt := 
  {imp| 
    tmp := x :;: 
    x := y :;: 
    y := tmp 
  }

theorem swap_correct (a b : ℕ) 
  : {* λ st => st "x" = a ∧ st "y" = b *}
      (swap)
    {* λ st => st "x" = b ∧ st "y" = a *} := by 
    apply Seq_rule'
    apply Seq_rule' 
    apply Assign_rule
    apply Assign_rule
    apply Assign_rule'
    simp [assertion_sub, updateEnv, evalExp]
    intros st H1 H2
    constructor 
    · 
      sorry 
    · 
      sorry 
     
