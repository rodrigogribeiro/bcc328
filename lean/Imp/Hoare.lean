import Mathlib.Data.Nat.Defs 
import Mathlib.Tactic.Basic 

import Aesop 

import Imp.Syntax
import Imp.Interpreter 
import Imp.Semantics 

-- assertions 

abbrev Assertion := Env → Prop 

-- Hoare logic 

def Hoare (P : Assertion)(s : Stmt)(Q : Assertion) :=
  ∀ (env env' : Env), P env → Eval env s env' → Q env'

macro "{*" P:term " *} " "(" s:term ")" " {* " Q:term " *}" : term =>
  `(Hoare $P $s $Q)

-- rules of Hoare logic 

theorem Skip_rule (P : Assertion) 
  : {* P *} (Stmt.Skip) {* P *}:= by 
  intros env env' Hp Hev
  cases Hev 
  exact Hp 

def assertion_sub (s : String) 
                  (e : IExp) 
                  (P : Assertion) : Assertion := 
  λ env => P (s |-> (evalExp e env) ; env)

macro P:term "[*" s:term "↦" e:term "*]" : term => 
  `(assertion_sub $s $e $P)

theorem Assign_rule (P : Assertion)
  s e :   {* P [* s ↦ e *] *}
            (Stmt.Assign s e) 
          {* P *} := by 
  intros env env' Hp H1
  cases H1
  rw [assertion_sub] at Hp
  rename_i He 
  rw [← He]
  assumption

theorem Seq_rule (P Q R : Assertion) s1 s2 
  : {* P *} (s1) {* Q *} → 
    {* Q *} (s2) {* R *} → 
    {* P *} (.Seq s1 s2) {* R *} := by
    intros H1 H2 env env' HP Hs 
    rcases Hs ; aesop

macro P:term "∧ True(" b:term ")" : term => 
  `(λ st => $P st ∧ evalExp $b st > 0)

macro P:term "∧ False(" b:term ")" : term => 
  `(λ st => $P st ∧ evalExp $b st = 0)

theorem If_rule (P Q : Assertion) b s1 s2 
  : {* P ∧ True(b) *} (s1) {* Q *} → 
    {* P ∧ False(b) *} (s2) {* Q *} → 
    {* P *} (.If b s1 s2) {* Q *} := by 
    intros H1 H2 
    intros env env' HPenv HEval 
    rcases HEval 
    · 
      rename_i v Hb Henv' 
      apply H1 <;> aesop
    · 
      rename_i H3 H4 
      apply H2 <;> aesop


theorem While_rule (P : Assertion) b s 
  : {* P ∧ True(b) *} (s) {* P *} → 
    {* P *} (.While b s) {* P ∧ False(b) *} := by 
    intros H1 env env' Hp HEval
    have H2 : ∃ x, x = Stmt.While b s := by 
      exists (Stmt.While b s)
    rcases H2 with ⟨ v , Heq ⟩ 
    rw [← Heq] at HEval 
    induction' HEval <;> try rcases Heq 
    · 
      rename_i env' H1
      aesop 
    · 
      rename_i env0 env1 env2 v Hexp Heval _IH2 _Heval1 IH1
      apply IH1 
      apply H1
      · 
        constructor 
        · 
          exact Hp
        · 
          rw [Hexp]
          simp 
      · 
        exact Heval 
      · 
        rfl 

    
