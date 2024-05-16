{-# LANGUAGE FlexibleInstances, IncoherentInstances #-}
module Lexer.Automata.Basic where

import Data.List (union, intersect)

class (Ord a, Show a) => State a
class (Ord a, Show a) => Symbol a
instance State a => State [a]


instance State Int
instance State String
instance Symbol Int
instance Symbol Char
instance Symbol String

disjunct :: Eq a => [a] -> [a] -> Bool
disjunct xs ys
  = null (xs `intersect` ys)

bigUnion :: Eq a => [[a]] -> [a]
bigUnion = foldr union []

powerSet :: Eq a => [a] -> [[a]]
powerSet [] = [[]]
powerSet (x : xs) = [x : ys | ys <- powerSet xs] ++ powerSet xs

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
  = let fx = f x
    in if fx == x then fx else fixpoint f fx
