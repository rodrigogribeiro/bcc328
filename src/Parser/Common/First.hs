module Parser.Common.First (first, First, firstForWord) where

import Data.List hiding (group)
import Data.Map (Map)
import qualified Data.Map as Map

import Parser.Common.Grammar


first :: Grammar -> [(Nonterminal, [Terminal])]
first g = Map.toList $ fixpoint (stepFirst g) (first0 g)

-- initialization of first-sets

first0 :: Grammar -> First
first0 g = Map.fromList $ map f (nonterminals g)
  where
    f nt = (nt , [])

-- stepping throught the productions of a grammar

stepFirst :: Grammar -> First -> First
stepFirst g current
  = step' (productions g) current
  where
    step' [] curr = curr
    step' (p:ps) curr
      = merge (step' ps curr)
              (leftHand p)
              (terminalsForRHS p)

    terminalsForRHS :: Production -> [Terminal]
    terminalsForRHS (Prod _ []) = [Lambda]
    terminalsForRHS (Prod _ [Symb Lambda]) = [Lambda]
    terminalsForRHS (Prod x (yj : ys))
      = case  yj of
          Symb terminal -> [terminal]
          Var nonterminal ->
            if Lambda `elem` first_iminus1 then
              terminalsForYj `union` terminalsForRHS (Prod x ys)
            else
              terminalsForYj
           where
             first_iminus1 = firstSetFor nonterminal current
             terminalsForYj = filter (/= Lambda) first_iminus1

firstForWord :: [Symbol] -> First -> [Terminal]
firstForWord [(Var nt)] ft = firstSetFor nt ft
firstForWord ((Symb t) : _ ) _ = [t]
firstForWord ((Var nt) : ss) ft =
  if Lambda `elem` firstSetFor nt ft then 
        firstMinusLambda `union` firstForWord ss ft
  else firstMinusLambda
       where firstMinusLambda = [x | x <- firstSetFor nt ft, x /= Lambda]
firstForWord _ _ = []

type First = Map Nonterminal [Terminal]

merge :: First -> Nonterminal -> [Terminal] -> First
merge m nt ts = Map.insertWith union nt ts m

firstSetFor :: Nonterminal -> First -> [Terminal]
firstSetFor nt m
  = case Map.lookup nt m of
      Nothing -> []
      Just ts -> ts
