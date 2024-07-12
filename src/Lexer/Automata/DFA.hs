{-# LANGUAGE FlexibleContexts #-}
module Lexer.Automata.DFA where

import Data.List (intersect, union, nub)
import Lexer.Automata.Basic

-- definition of a type for DFAs

data DFA a b
  = DFA {
      dfaStates :: [a]
    , dfaSigma   :: [b]
    , dfaTransition :: [((a,b),a)]
    , dfaStart  :: a
    , dfaFinals :: [a]
    } deriving (Eq, Show)

-- computing reachable states

reachStep :: State a => DFA a b -> [a] -> [a]
reachStep m es
  = bigUnion $ map step es
  where
    step e = [e' | ((d,_),e') <- dfaTransition m, e == d] `union` es

reachable :: State a => DFA a b -> [a]
reachable m = nub $ fixpoint (reachStep m) [dfaStart m]

elimDeadStates :: State a => DFA a b -> DFA a b
elimDeadStates m
  = DFA states'
        (dfaSigma m)
        transitions'
        (dfaStart m)
        finals'
    where
      states' = reachable m
      finals' = dfaFinals m `intersect` states'
      transitions' = filter onlyReachable $ dfaTransition m
      onlyReachable ((e,_),e') = e `elem` states' && e' `elem` states'

-- basic transition

dfaDelta :: (State a, Symbol b) => DFA a b -> a -> b -> a
dfaDelta m s c
  = case lookup (s,c) (dfaTransition m) of
      Just s' -> s'
      Nothing -> error "Transition function is not total!"

-- definition of acceptance

dfaDeltaStar :: (State a, Symbol b) => DFA a b -> a -> [b] -> a
dfaDeltaStar _ s [] = s
dfaDeltaStar m s (c : cs) = dfaDeltaStar m (dfaDelta m s c) cs

dfaAccept :: (State a, Symbol b) => DFA a b -> [b] -> Bool
dfaAccept m s
  = dfaDeltaStar m (dfaStart m) s `elem` (dfaFinals m)
