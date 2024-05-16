module Lexer.Automata.NFALambda where

import Data.List   (union,intersect)

import Lexer.Automata.Basic
import Lexer.Automata.NFA

-- transition type

type Transition a b = ((a, Maybe b), [a])

-- type for automata with lambda transitions.

data NFALam a b
  = NFALam {
      nfaLamStates :: [a]
    , nfaLamSigma   :: [b]
    , nfaLamTransition :: [Transition a b]
    , nfaLamStart  :: [a]
    , nfaLamFinals :: [a]
    } deriving (Eq, Show)


-- lambda closure

stepClosure :: (State a, Symbol b) => NFALam a b -> [a] -> [a]
stepClosure m es
  = es `union` concat es'
    where
      ts = nfaLamTransition m
      es' = [e' | ((e,Nothing),e') <- ts  -- getting only lambda transitions
                , e `elem` es             -- only transitions with the right domain.
                ]

lambdaClosure :: (State a, Symbol b) => NFALam a b -> [a] -> [a]
lambdaClosure m es
  = fixpoint (stepClosure m) es


-- acceptance

nfaLamDelta :: (State a, Symbol b) => NFALam a b -> a -> b -> [a]
nfaLamDelta m s c
  = case lookup (s, Just c) (nfaLamTransition m) of
      Just s' -> s'
      Nothing -> []


nfaLamDeltaStar :: (State a, Symbol b) => NFALam a b -> [a] -> [b] -> [a]
nfaLamDeltaStar _ [] _ = []
nfaLamDeltaStar m s [] = lambdaClosure m s
nfaLamDeltaStar m s (c : cs)
  = nfaLamDeltaStar m (bigUnion (map delta s')) cs
    where
      s' = lambdaClosure m s
      delta e = nfaLamDelta m e c

nfaLamAccept :: (State a, Symbol b) => NFALam a b -> [b] -> Bool
nfaLamAccept m cs
  = not $ null $ nfaLamDeltaStar m start cs `intersect` nfaLamFinals m
    where
      start = nfaLamStart m


-- converting NFALambda into NFA

eliminateLam :: (State a, Symbol b) => NFALam a b -> NFA a b
eliminateLam m
  = NFA (nfaLamStates m)
        (nfaLamSigma m)
        (concatMap trans ts)
        (lambdaClosure m (nfaLamStart m))
        (nfaLamFinals m)
    where
      ts = nfaLamTransition m
      trans ((e, Just c), es) = [((e, c), lambdaClosure m es)]
      trans _ = []


test :: NFALam Int Char
test = starNFA (chrNFALam "01" '0')

-- base cases for NFALam

emptyNFALam :: Symbol b => [b] -> NFALam Int b
emptyNFALam sigma
  = NFALam [0]
           sigma
           []
           [0]
           []

lambdaNFALam :: Symbol b => [b] -> NFALam Int b
lambdaNFALam sigma
  = NFALam [0]
           sigma
           []
           [0]
           [0]

chrNFALam :: Symbol b => [b] -> b -> NFALam Int b
chrNFALam sigma c
  = NFALam [0,1]
           sigma
           [((0, Just c),[1])]
           [0]
           [1]

-- combinators

renameTrans :: (Int -> Int) -> [((Int, Maybe b), [Int])] -> [((Int, Maybe b), [Int])]
renameTrans f xs
  = map (\ ((e, c), es) -> ((f e, c), map f es)) xs

unionNFA :: Symbol b => NFALam Int b -> NFALam Int b -> NFALam Int b
unionNFA m1 m2
  = NFALam (0 : states1 `union` states2 `union` [f])
           (sigma1 `union` sigma2)
           transitions
           [0]
           [f]
    where
      states1 = map (+ 1) (nfaLamStates m1)
      n1 = length states1 + 1
      states2 = map (+ n1) (nfaLamStates m2)
      start1 = map (+ 1) (nfaLamStart m1)
      start2 = map (+ n1) (nfaLamStart m2)
      sigma1 = nfaLamSigma m1
      sigma2 = nfaLamSigma m2
      f = n1 + length states2 + 1
      finals1 = map (+ 1) (nfaLamFinals m1)
      finals2 = map (+ n1) (nfaLamFinals m2)
      starting = map (\ s -> ((0, Nothing), [s]))
                     (start1 `union` start2)
      finals   = map (\ s -> ((s, Nothing), [f]))
                     (finals1 `union` finals2)
      trans1 = map (\ ((e,c),e1) -> ((e + 1,c), map (+1) e1))
                   (nfaLamTransition m1)
      trans2 = map (\ ((e,c),e1) -> ((e + n1,c), map (+ n1) e1))
                   (nfaLamTransition m2)
      transitions = concat [ starting
                           , trans1
                           , trans2
                           , finals
                           ]


concatNFA :: Symbol b => NFALam Int b -> NFALam Int b -> NFALam Int b
concatNFA m1 m2
  = NFALam (states1 `union` states2)
           (sigma1 `union` sigma2)
           transitions
           (nfaLamStart m1)
           finals2
    where
      states1 = nfaLamStates m1
      n1 = length states1 + 1
      sigma1 = nfaLamSigma m1
      sigma2 = nfaLamSigma m2
      states2 = map (+ n1) (nfaLamStates m2)
      start2 = map (+ n1) (nfaLamStart m2)
      finals1 = nfaLamFinals m1
      finals2 = map (+ n1) (nfaLamFinals m2)
      glue = map (\ (f,s) -> ((f,Nothing),[s]))
                 (cartesianProduct finals1 start2)
      trans2 = map (\ ((e,c),e1) -> ((e + n1,c), map  (+ n1) e1))
                   (nfaLamTransition m2)
      transitions = concat [ nfaLamTransition m1
                           , glue
                           , trans2
                           ]


starNFA :: Symbol b => NFALam Int b -> NFALam Int b
starNFA m
  = NFALam states
           (nfaLamSigma m)
           transitions
           [0]
           newfinals
    where
      states = 0 : map (+ 1) (nfaLamStates m)
      newfinals = 0 : map (+ 1) (nfaLamFinals m)
      trans = map (\((e,c),e1) -> ((e + 1, c), map (+1) e1))
                  (nfaLamTransition m)
      starts = map (\ e -> ((0, Nothing), [e]))
                   (map (+1) $ nfaLamStart m)
      loop = map (\ e -> ((e, Nothing), [0]))
                 newfinals
      transitions = concat [ starts
                           , trans
                           , loop
                           ]
