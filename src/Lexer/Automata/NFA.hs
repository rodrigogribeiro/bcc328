module Lexer.Automata.NFA where

import Data.List (intersect,nub)
import Lexer.Automata.Basic
import Lexer.Automata.DFA

-- definition of a NFA type

data NFA a b
  = NFA {
      nfaStates :: [a]
    , nfaSigma   :: [b]
    , nfaTransition :: [((a,b),[a])]
    , nfaStart  :: [a]
    , nfaFinals :: [a]
    } deriving (Eq, Show)

-- basic transition

nfaDelta :: (State a, Symbol b) => NFA a b -> a -> b -> [a]
nfaDelta m s c
  = case lookup (s,c) (nfaTransition m) of
      Just s' -> s'
      Nothing -> []

-- definition of acceptance

nfaDeltaStar :: (State a, Symbol b) => NFA a b -> [a] -> [b] -> [a]
nfaDeltaStar _ s [] = s
nfaDeltaStar m s (c : cs) = nfaDeltaStar m ss cs
  where
    ss = bigUnion $ map (\ e -> nfaDelta m e c) s

nfaAccept :: (State a, Symbol b) => NFA a b -> [b] -> Bool
nfaAccept m s
  = not $ null $ nfaDeltaStar m (nfaStart m) s `intersect` (nfaFinals m)

-- embedding a DFA into a NFA

dfaToNFA :: DFA a b -> NFA a b
dfaToNFA m
  = NFA (dfaStates m)
        (dfaSigma m)
        (map trans (dfaTransition m))
        [dfaStart m]
        (dfaFinals m)
  where
    trans ((s,c),s') = ((s,c),[s'])

-- reverse NFA

reverseNFA :: (State a, Symbol b) => NFA a b -> NFA a b
reverseNFA m
  = NFA (nfaStates m)
        (nfaSigma m)
        (groupByDom $ concatMap rev (nfaTransition m))
        (nfaFinals m)
        (nfaStart m)
    where
      rev ((s,c), ss) = map (\ x -> ((x,c), [s])) ss


lookupList :: Eq a => [(a,b)] -> a -> Maybe ([(a,b)],[(a,b)])
lookupList xs k = go xs k []
  where
    go [] _ _ = Nothing
    go (p@(x,_) : ks) x' ac
      | x == x' = Just (p : ac, ks)
      | otherwise = go ks x' (p : ac)

groupByDom :: Eq a => [(a,[b])] -> [(a,[b])]
groupByDom = foldr step []
  where
    step (d,img) ac
      = case lookupList ac d of
          Nothing  -> (d, img) : ac
          Just ((_, imgs) : ds, rs) ->
            (d, img ++ imgs) : ds ++ rs
          Just ([], _) -> ac


-- subset construction

subset :: (State a, Symbol b) => NFA a b -> DFA [a] b
subset m
  = DFA states
        sigma
        (subsetTransitions m)
        (nfaStart m)
        finals
    where
      states = subsetStates m
      sigma = nfaSigma m
      finals = filter (\ e -> not $ disjunct e (nfaFinals m)) states

subsetTransitions :: (State a, Symbol b) => NFA a b -> [(([a], b), [a])]
subsetTransitions m
  = map (\ d@(es', c) -> (d, bigUnion $ map (\ e -> nfaDelta m e c) es')) dom
  where
    es = subsetStates m
    sig = nfaSigma m
    dom = cartesianProduct es sig

subsetStates :: (State a, Symbol b) => NFA a b -> [[a]]
subsetStates m
  = fixpoint step [nfaStart m]
    where
      step es = nub $ es ++ [nfaDelta m e c | es1 <- es, e <- es1,
                                              c <- nfaSigma m]
