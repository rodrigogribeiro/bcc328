module Lexer.Regex.Alternative where

import Data.List (sort, union)
import Data.Set (Set)
import qualified Data.Set as Set

import Lexer.Regex.Syntax

-- alternative representation of regex

data RE a
  = Emp
  | Lam
  | Sym a
  | Union (Set (RE a))
  | Conc [RE a]
  | S (RE a)
  deriving (Eq, Ord, Show)

regexToRE :: Ord a => Regex a -> RE a
regexToRE Empty = Emp
regexToRE Lambda = Lam
regexToRE (Chr c) = Sym c
regexToRE (Choice e1 e2)
  = reUnion (regexToRE e1) (regexToRE e2)
regexToRE (Cat e1 e2)
  = reConcat (regexToRE e1) (regexToRE e2)
regexToRE (Star e1)
  = reStar (regexToRE e1)

reUnion :: Ord a => RE a -> RE a -> RE a
reUnion Emp e2 = e2
reUnion e1 Emp = e1
reUnion (Union s1) (Union s2)
  = Union (s1 `Set.union` s2)
reUnion e1 e2 = Union (Set.fromList [e1,e2])

reConcat :: Ord a => RE a -> RE a -> RE a
reConcat Emp _ = Emp
reConcat _ Emp = Emp
reConcat Lam e2 = e2
reConcat e1 Lam = e1
reConcat (Conc e1) (Conc e2) = Conc $ sort (e1 `union` e2)
reConcat e1 e2 = Conc $ sort [e1, e2]

reStar :: Ord a => RE a -> RE a
reStar Emp = Lam
reStar Lam = Lam
reStar (S e) = S e
reStar e1 = S e1


-- nullability

reNull :: RE a -> Bool
reNull Emp = False
reNull Lam = True
reNull (Sym _) = False
reNull (Union s1)
  = any reNull $ Set.toList s1
reNull (Conc s1)
  = all reNull s1
reNull (S _) = True

-- derivative

reDeriv :: Ord a => RE a -> a -> RE a
reDeriv Emp _ = Emp
reDeriv Lam _ = Emp
reDeriv (Sym c) c'
  | c == c' = Lam
  | otherwise = Emp
reDeriv (Union s1) c
  = Union $ Set.map (flip reDeriv c) s1
reDeriv (Conc s1) c
  = Conc $ map (flip reDeriv c)


-- smart constructors

setUnion :: Ord a => Set (RE a) -> Set (RE a)
setUnion ss
  = Set.fromList $ foldr reUnion Emp $ Set.toList ss

listCat :: Ord a => [RE a] -> [RE a]
listCat = foldr step []
  where
    step e ac
      | null ac = [e]
      | otherwise =
