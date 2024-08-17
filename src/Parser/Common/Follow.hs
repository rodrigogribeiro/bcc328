module Parser.Common.Follow (follow) where

import Parser.Common.Grammar
import Parser.Common.First 

import Data.List hiding (group)
import Data.Map (Map)
import qualified Data.Map as Map


follow :: Grammar -> [(Nonterminal, [Terminal])]
follow g
  = Map.toList $ fixpoint (stepFollow g m) (follow0 g)
  where
    m = Map.fromList $ first g

follow0 :: Grammar -> Follow
follow0 g = Map.fromList $ map f (nonterminals g)
  where
    f nt = if nt == start g then (nt, [Dollar]) else (nt, [])

stepFollow :: Grammar -> First -> Follow -> Follow
stepFollow g firstG current
  = step' (productions g) firstG current
    where
      step' [] _ curr = curr
      step' (p : ps) fG curr
        = mergeTerminals p (step' ps fG curr)
       where
         mergeTerminals (Prod _ []) = undefined
         mergeTerminals (Prod l (s : ss)) = merge' l [] s ss
         merge' a w1 nt@(Var b) w2@(w21 : w2s) fMinus1
           = merge (merge' a (w1 ++ [nt]) w21 w2s fMinus1) b new
           where
             firstW2 = [x | x <- firstForWord w2 fG, x /= Lambda]
             new = if Lambda `elem` firstForWord w2 fG then
                     followSetFor a fMinus1 `union` firstW2
                   else firstForWord w2 fG
         merge' a _ (Var b) [] fMinus1
           = merge fMinus1 b (followSetFor a fMinus1)
         merge' _ _  (Symb _) [] fMinus1 = fMinus1
         merge' a w1 (Symb t) (w2 : w2s) fMinus1
           = merge' a (w1 ++ [Symb t]) w2 w2s fMinus1


type Follow = Map Nonterminal [Terminal]

merge :: Follow -> Nonterminal -> [Terminal] -> Follow
merge m nt ts = Map.insertWith union nt ts m

followSetFor :: Nonterminal -> Follow -> [Terminal]
followSetFor nt m
  = case Map.lookup nt m of
      Nothing -> []
      Just ts -> ts


