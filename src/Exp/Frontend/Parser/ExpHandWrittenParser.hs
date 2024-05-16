module Exp.Frontend.Parser.ExpHandWrittenParser where

import Control.Applicative

import Exp.Frontend.Lexer.Token
import Exp.Syntax.Syntax 

-- top level parser function  

parser :: [Token] -> Maybe Exp 
parser ts 
  = fst <$> runParser expParser ts

-- parsing expressions 

expParser :: Parser Exp 
expParser 
  = chainr termParser plus 
    where 
      plus = (:+:) <$ satisfy isAdd 
      isAdd (Token _ TPlus) = True 
      isAdd _ = False 

termParser :: Parser Exp 
termParser 
  = chainr factorParser mul 
    where 
      mul = (:*:) <$ satisfy isMul 
      isMul (Token _ TTimes) = True 
      isMul _ = False 

factorParser :: Parser Exp 
factorParser 
  = parens expParser <|> numberParser 

numberParser :: Parser Exp 
numberParser 
  = f <$> satisfy isNumber 
    where 
      isNumber (Token _ (TNumber _)) = True 
      isNumber _ = False 

      f (Token _ (TNumber n)) = EInt n
      f _ = EInt 0

-- basic parsers 

satisfy :: (Token -> Bool) -> Parser Token 
satisfy p 
  = Parser $ \ ts -> 
      case ts of 
        [] -> Nothing 
        (t' : ts') -> if p t' then Just (t', ts')
                      else Nothing

symbol :: Token -> Parser Token 
symbol t = satisfy (== t)

parens :: Parser a -> Parser a
parens p 
  = f <$> pParen TLParen <*> p <*> pParen TRParen 
    where 
      f _ x _ = x 
      pParen c = satisfy (\ t -> c == (lexeme t))

-- basic functions for defining recursive descendent parser 

newtype Parser a 
  = Parser { runParser :: [Token] -> Maybe (a, [Token]) }

instance Functor Parser where 
  fmap f (Parser p) 
    = Parser $ \ ts -> 
          do 
             (x,ts') <- p ts
             pure (f x, ts')

instance Applicative Parser where 
  pure x = Parser $ \ ts -> Just (x, ts)
  (Parser p1) <*> (Parser p2)
    = Parser $ \ ts -> do {
                        (f, ts1) <- p1 ts ;
                        (x, ts2) <- p2 ts1 ; 
                        pure (f x, ts2)
                      }
instance Alternative Parser where 
  empty = Parser $ \ _ -> Nothing 
  (Parser p1) <|> (Parser p2) 
    = Parser $ \ ts -> case p1 ts of 
                          Just (x, ts') -> Just (x, ts')
                          Nothing -> p2 ts 

chainr :: Parser a -> Parser (a -> a -> a) -> Parser a 
chainr pe po 
  = h <$> many (j <$> pe <*> po) <*> pe 
    where 
      j x op = (x `op`)
      h fs x = foldr ($) x fs

{-
class Functor f where
   fmap :: (a -> b) -> f a -> f b
-}
