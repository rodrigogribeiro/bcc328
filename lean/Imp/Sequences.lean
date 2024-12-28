import Mathlib.Tactic 
import Aesop 

section SEQUENCES

  -- reflexive and transitive closure 
  
  inductive star {A : Type}{R : A → A → Prop} : A → A → Prop where 
  | srefl x : star x x 
  | sstep x y z : R x y → star y z → star x z

  theorem starone {A : Type}{R : A → A → Prop}{x y : A} 
    : R x y → @star A R x y := by 
      intros H
      apply star.sstep 
      exact H 
      apply star.srefl 

  lemma startrans {A : Type}{R : A → A → Prop}{x y} 
    : @star A R x y → ∀ z, @star A R y z → @star A R x z := by 
      intros H1
      induction H1 
      · 
        intros z H2
        aesop 
      · 
        intros z H2
        rename_i H3 _H4 IH
        apply star.sstep 
        exact H3 
        apply IH 
        assumption 

   

end SEQUENCES
