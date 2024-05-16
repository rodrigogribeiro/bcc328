module Lexer.Regex.ToDFA where

import Lexer.Automata.Basic hiding (State)
import Lexer.Automata.DFA
import Lexer.Automata.NFA
import Lexer.Automata.NFALambda
import Lexer.Automata.Minimization
import Lexer.Automata.Rename
import Lexer.Regex.Syntax

-- building an equivalent automata from a Regex

regexToNFALam :: Symbol b => Regex b -> NFALam Int b
regexToNFALam e
  = regexToNFALam' e (sigmaFrom e)

regexToNFALam' :: Symbol b => Regex b -> [b] -> NFALam Int b
regexToNFALam' Empty sigma
  = emptyNFALam sigma
regexToNFALam' Lambda sigma
  = lambdaNFALam sigma
regexToNFALam' (Chr c) sigma
  = chrNFALam sigma c
regexToNFALam' (Choice e1 e2) sigma
  = unionNFA (regexToNFALam' e1 sigma)
             (regexToNFALam' e2 sigma)
regexToNFALam' (Cat e1 e2) sigma
  = concatNFA (regexToNFALam' e1 sigma)
              (regexToNFALam' e2 sigma)
regexToNFALam' (Star e1) sigma
  = starNFA (regexToNFALam' e1 sigma)

regexToNFA :: Symbol b => Regex b -> NFA Int b
regexToNFA = eliminateLam . regexToNFALam

regexToDFA :: Symbol b => Regex b -> DFA Int b
regexToDFA e = rebuildStates m
    where
      dfa = elimDeadStates $ subset $ regexToNFA e
      m = minimize dfa

