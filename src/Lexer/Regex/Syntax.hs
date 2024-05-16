module Lexer.Regex.Syntax where

import Data.List (union)
import Lexer.Automata.Basic

-- definition of regular expression syntax

data Regex a
  = Empty
  | Lambda
  | Chr a
  | Choice (Regex a) (Regex a)
  | Cat (Regex a) (Regex a)
  | Star (Regex a)
  deriving (Eq, Ord, Show)

-- getting the alphabet from a regex

sigmaFrom :: Symbol a => Regex a -> [a]
sigmaFrom Empty = []
sigmaFrom Lambda = []
sigmaFrom (Chr c) = [c]
sigmaFrom (Choice e1 e2)
  = sigmaFrom e1 `union` sigmaFrom e2
sigmaFrom (Cat e1 e2)
  = sigmaFrom e1 `union` sigmaFrom e2
sigmaFrom (Star e1)
  = sigmaFrom e1

