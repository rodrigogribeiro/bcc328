module Lexer.Regex.Derivative.ToDFA where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (union)

import Lexer.Automata.Basic hiding (State)
import Lexer.Automata.DFA
import Lexer.Regex.Syntax
import Lexer.Regex.Derivative.Antimirov
import Lexer.Regex.Derivative.Brzozowski (nullable)

type State a = Set (Regex a)

regexToDFA :: (Symbol a, Ord a) => Regex a -> DFA (State a) a
regexToDFA e
  = DFA states sig trans s finals
  where
    sig = sigmaFrom e
    s = Set.singleton e
    finals = filter (any nullable . Set.toList) states
    (states, trans) = build sig ([s],[])

type Conf a = ([State a], [((State a, a), State a)])

build :: (Symbol a, Ord a) => [a] -> Conf a -> Conf a
build sigma (ss, ts)
  = let
      (ss1, ts1) = foldr (\ c (st, tt) ->
                             let ts' = map (\ e1 -> ((e1, c), deriv e1 c)) st
                                 st' = st `union` map snd ts'
                                 tt' = ts' `union` tt
                             in (st', tt')) (ss, ts) sigma
    in if ss1 == ss && ts == ts1 then (ss,ts)
       else build sigma (ss1, ts1)
