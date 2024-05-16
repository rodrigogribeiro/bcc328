module Lexer.Regex.Derivative.Antimirov where

import Data.Set (Set)
import qualified Data.Set as Set

import Lexer.Regex.Syntax
import Lexer.Regex.Derivative.Brzozowski (nullable)

partialDeriv :: Ord a => Regex a -> a -> Set (Regex a)
partialDeriv Empty _ = Set.empty
partialDeriv Lambda _ = Set.empty
partialDeriv (Chr c) c'
  | c == c' = Set.singleton Lambda
  | otherwise = Set.empty
partialDeriv (Choice e1 e2) c
  = partialDeriv e1 c `Set.union` partialDeriv e2 c
partialDeriv (Cat e1 e2) c
  | nullable e1 = Set.union (Set.map (\ e -> Cat e e2) $ partialDeriv e1 c)
                            (partialDeriv e2 c)
  | otherwise = Set.map (\ e -> Cat e e2) (partialDeriv e1 c)
partialDeriv (Star e1) c
  = Set.map (\ e -> Cat e (Star e1)) (partialDeriv e1 c)


deriv :: Ord a => Set (Regex a) -> a -> Set (Regex a)
deriv ss c = Set.foldr (\ e ac -> Set.union (partialDeriv e c) ac)
                       Set.empty
                       ss
