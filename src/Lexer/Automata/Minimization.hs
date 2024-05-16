module Lexer.Automata.Minimization where

import Lexer.Automata.Basic
import Lexer.Automata.DFA
import Lexer.Automata.NFA
import Lexer.Automata.Rename

-- definition of Brzozowski minimization algorithm

step :: (State a, Symbol b) => DFA a b -> DFA [a] b
step = elimDeadStates . subset . reverseNFA . dfaToNFA


-- minimization is just two steps

minimize :: (State a, Symbol b) => DFA a b -> DFA Int b
minimize = rebuildStates . step . step
