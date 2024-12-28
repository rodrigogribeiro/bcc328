import Mathlib.Data.Nat.Defs 
import Mathlib.Tactic.Basic 

import Aesop 

import Imp.Hoare
import Imp.Syntax
import Imp.Interpreter 
import Imp.Semantics 

-- some derived rules for Hoare logic 

theorem Consequence_left (P') {P Q s} 
  : {* P *}(s) {* Q *} → 
    (∀ st, P' st → P st) → 
    {* P' *}(s) {* Q *} := by 
    intros H1 H2
    apply Consequence_rule 
    exact H1 
    exact H2 
    intros _st H3 ; exact H3 

theorem Consequence_right (Q){P Q' s}
  : {* P *}(s) {* Q *} → 
    (∀ st, Q st → Q' st) → 
    {* P *} (s) {* Q' *} := by 
    
    intros H1 H2 
    apply Consequence_rule 
    exact H1 
    intros _ H1 ; exact H1 
    exact H2 

theorem Skip_rule' {P Q} 
  : (∀ st, P st → Q st) → 
    {* P *} (Stmt.Skip) {* Q *} := by 
    intros H1 
    apply Consequence_right 
    apply Skip_rule 
    assumption 

theorem Assign_rule' {P Q x a} 
  : (∀ st, P st → Q (x |-> (evalExp a st) ; st)) → 
    {* P *}(Stmt.Assign x a) {* Q *} := by 
    intros H env env' Hp HEval 
    rcases HEval 
    rename_i v Heq 
    specialize H env 
    rw [Heq] at H 
    apply H 
    exact Hp 

theorem Seq_rule' {P Q R s1 s2} 
  : {* Q *} (s2) {* R *} → 
    {* P *} (s1) {* Q *} → 
    {* P *} (Stmt.Seq s1 s2) {* R *} := by 
    intros H1 H2 env env' Hp Heval 
    rcases Heval 
    rename_i env1 H4 H5  
    apply H1 <;> try assumption 
    apply H2 <;> try assumption 

theorem While_rule' {P Q a s}(I)
  (H1 : {* I .&. True(a) *} (s) {* I *}) 
  (H2 : (∀ st, P st → I st)) 
  (H3 : ∀ st, evalExp a st = 0 → I st → Q st) : 
    {* P *}(Stmt.While a s){* Q *} := by 
    apply Consequence_rule
    apply While_rule
    apply H1 
    assumption
    intros st H4
    specialize H3 st 
    simp [assert_and] at H4
    rcases H4 with ⟨ H41 , H42 ⟩ 
    apply H3 <;> try assumption 
