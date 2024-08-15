module Parser.Recursive.SimpleCombinators ( module Parser.Recursive.SimpleCombinators
                                          , Alternative (..)
                                          ) where

-- implementation of the parser library of the
-- article: Monadic Parsing in Haskell
-- this library supports monadic and applicative parsers

import Control.Monad
import Control.Applicative

import Data.Char

-- definition of parser type

newtype Parser s a =
  Parser { runParser :: [s] -> [(a,[s])] }

-- a parser is a functor

instance Functor (Parser s) where
  fmap f p = Parser (\ s -> [(f x, s') | (x,s') <- runParser p s]) 

-- a parser is a applicative functor

instance Applicative (Parser s) where
  pure  a = Parser (\ts -> [(a, ts)])
  p1 <*> p2 = Parser (\ s -> [(f x, s2) | (f, s1) <- runParser p1 s,
                                          (x, s2) <- runParser p2 s1])

-- a parser is a alternative

instance Alternative (Parser t) where
  empty = Parser (\ _ -> [])
  p1 <|> p2 = Parser (\ s -> runParser p1 s ++ runParser p2 s)

-- a parser is a monad

instance Monad (Parser t) where
  return =  pure
  p >>= f  = Parser (\ts -> concat [ runParser (f a) cs' |
                                     (a,cs') <- runParser p ts ])

-- a parser is a monad with zero

instance MonadPlus (Parser t) where
  mzero = Parser (\_ -> [])
  p1 `mplus` p2 = Parser (\cs -> runParser p1 cs ++ runParser p2 cs)

-- a parser that always succeeds

succeed :: a -> Parser s a
succeed x = Parser (\ts -> [(x,ts)])

-- the combinators

item :: Parser t t
item = Parser (\ ts ->
                 case ts of
                   []     -> []
                   (c:cs) -> [(c,cs)])

sat :: (t -> Bool) -> Parser t t
sat p = do
          t <- item
          if p t then return t else mzero

-- parsing a single symbol

symbol :: Eq s => s -> Parser s s
symbol c = sat (c ==)

-- parsing a token

token :: Eq s => [s] -> Parser s [s]
token = mapM symbol 

-- parsing a single digit

digit :: Parser Char Int
digit = f <$> sat isDigit
  where
    f c = ord c - ord '0'

-- optionally executes a parser

option :: Parser s a -> a -> Parser s a
option p v = p <|> succeed v

-- recursion parsers

many1    :: Parser s a -> Parser s [a]
many1 p  =  list <$> p <*> many p

-- packing a parser

pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p r q  =  pi32 <$> p <*> r <*> q 

-- parsing a list with a separator

listOf      :: Parser s a -> Parser s b -> Parser s [a]
listOf p s  =  list <$> p <*> many (pi22 <$> s <*> p)

-- auxiliary functions

determ  ::  Parser s b -> Parser s b
determ p = Parser (\ ts ->
                     case (runParser p ts) of
                       []    -> []
                       (x:_) -> [x] )


greedy, greedy1  ::  Parser s b -> Parser s [b]
greedy   =  determ . many
greedy1  =  determ . many1

list :: a -> [a] -> [a]
list x xs  =  x:xs

pi22 :: a -> b -> b
pi22 _ y    =  y

pi32 :: a -> b -> c -> b
pi32 _ y _  =  y

-- Applications of EBNF combinators

natural  :: Parser Char Int
natural  
  =  foldl (\ a b -> a * 10 + b) 0 <$> many1 digit

integer  ::  Parser Char Int
integer  =  (const negate <$> (symbol '-')) `option` id  <*>  natural 

identifier :: Parser Char String
identifier =  list <$> sat isAlpha <*> greedy (sat isAlphaNum)

parens :: Parser Char a -> Parser Char a
parens p  =  pack (symbol '(') p (symbol ')')

commaList    :: Parser Char a -> Parser Char [a]
commaList p  =  listOf p (symbol ',')

spaces :: Parser Char String
spaces = greedy (sat isSpace)

-- Chain expression combinators

chainr  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po  =  h <$> many (j <$> pe <*> po) <*> pe
  where j x op  =  (x `op`)
        h fs x  =  foldr ($) x fs

chainl  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe po  =  h <$> pe <*> many (j <$> po <*> pe)
  where j op x  =  (`op` x)
        h x fs  =  foldl (flip ($)) x fs


-- Combinators for repetition 

psequence         :: [Parser s a] -> Parser s [a]
psequence []      =  succeed []
psequence (p:ps)  =  list <$> p <*> psequence ps

psequence'  :: [Parser s a] -> Parser s [a]
psequence'  =  foldr f (succeed [])
  where  f p q = list <$> p <*> q

choice  :: [Parser s a] -> Parser s a
choice  =  foldr (<|>) mzero

-- End by combinator

endBy :: Parser s t -> Parser s c -> Parser s [t]
endBy p s = greedy (p >>= \x -> s >> return x)
