{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Parser.PEG.ParsingExp where

import Control.Applicative
import Control.Monad
import Data.Char
import Prelude hiding (not, and)

-- definition of the parsing result

data Result s a
  = Pure a           -- did not consume anything. We can backtrack.
  | Commit s a       -- remaining input and result.
  | Fail String Bool -- true if consume any input
  deriving (Show, Functor)

-- definition of a parsing expression

newtype PExp s a
  = PExp {
      runPExp :: s -> Result s a
    } deriving Functor

-- input stream

class Stream a where
  anyChar :: PExp a Char

instance Stream String where
  anyChar = PExp $ \ d ->
    case d of
      (x : xs) -> Commit xs x
      []       -> Fail "eof" False

-- definition of applicative instance

instance Applicative (PExp s) where
  pure x = PExp $ \ _ -> Pure x
  (PExp efun) <*> (PExp earg)
    = PExp $ \ d ->
        case efun d of
          Pure f   -> f <$> earg d
          Fail s c -> Fail s c
          Commit d' f ->
            case earg d' of
              Pure a -> Commit d' (f a)
              Fail s' _ -> Fail s' True
              Commit d'' a -> Commit d'' (f a)

-- definition of the monad instance

instance Monad (PExp s) where
  return = pure
  (PExp m) >>= f
    = PExp $ \ d ->
        case m d of
          Pure a -> runPExp (f a) d
          Commit d' a ->
            case runPExp (f a) d' of
              Pure b   -> Commit d' b
              Fail s _ -> Fail s True
              commit   -> commit
          Fail s c -> Fail s c

-- definition of a try operator

try :: PExp d a -> PExp d a
try (PExp m) = PExp $ \ d ->
  case m d of
    Fail s _ -> Fail s False
    x        -> x

-- ordered choice

instance Alternative (PExp d) where
  (PExp e1) <|> (PExp e2) = PExp $ \ d ->
    case e1 d of
      Fail _ _ -> e2 d
      x        -> x
  empty = PExp $ \ _ -> Fail "empty" False

(</>) :: PExp d a -> PExp d a -> PExp d a
e1 </> e2 = try e1 <|> e2

-- basic symbols

satisfy :: Stream d => (Char -> Bool) -> PExp d Char
satisfy p = do
  x <- anyChar
  x <$ guard (p x)

symbol :: Stream d => Char -> PExp d Char
symbol c = satisfy (c ==)

lambda :: Stream d => a -> PExp d a
lambda v = PExp $ \ d -> Commit d v

-- star operator

star :: Stream d => PExp d a -> PExp d [a]
star e1 = PExp $ \ d ->
  case runPExp e1 d of
    Fail _ _ -> Commit d []
    Pure _ -> Fail "Nullable star" False
    Commit d' v ->
      case runPExp (star e1) d' of
        Fail _ _ -> Commit d []
        Commit d'' vs -> Commit d'' (v : vs)
        Pure _ -> Fail "Nullable star" False

-- not predicate

not :: Stream d => PExp d a -> PExp d ()
not e = PExp $ \ d ->
  case runPExp e d of
    Fail _ _ -> Pure ()
    _        -> Fail "not" False

and :: Stream d => PExp d a -> PExp d ()
and e 
  = not $ not e


