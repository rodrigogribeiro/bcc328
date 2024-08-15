import Mathlib.Data.Nat.Defs 
import Mathlib.Tactic.Basic 

import Aesop 

import Imp.Syntax
import Imp.Interpreter 
import Imp.Semantics 

-- Hoare logic 

def Hoare (P : Env → Prop)(s : Stmt)(Q : Env → Prop) :=
  ∀ (env env' : Env), P env → Eval env s env' → Q env'

macro "{*" P:term " *} " "(" s:term ")" " {* " Q:term " *}" : term =>
  `(Hoare $P $s $Q)

-- rules of Hoare logic 

theorem Skip_rule (P : Env → Prop) 
  : {* P *} (Stmt.Skip) {* P *}:= by 
  intros env env' Hp Hev
  cases Hev 
  exact Hp 

def assertion_sub (s : String) 
                  (e : IExp) 
                  (P : Env → Prop) : Env → Prop := 
  λ env => P (s |-> (evalExp e env) ; env)

macro P:term "[*" s:term "↦" e:term "*]" : term => 
  `(assertion_sub $s $e $P)

theorem Assign_rule (P : Env → Prop)
  s e :   {* P [* s ↦ e *] *}
            (Stmt.Assign s e) 
          {* P *} := by 
  intros env env' Hp H1
  cases H1
  rw [assertion_sub] at Hp
  rename_i He 
  rw [← He]
  assumption

theorem Seq_rule (P Q R : Env → Prop) s1 s2 
  : {* P *} (s1) {* Q *} → 
    {* Q *} (s2) {* R *} → 
    {* P *} (.Seq s1 s2) {* R *} := by
    intros H1 H2 env env' HP Hs 
    rcases Hs ; aesop 

