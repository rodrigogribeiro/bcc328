module Parser.LL.Expr (exprGrammar, exprLex) where

import Prelude hiding (lex)
import Parser.Common.Grammar

exprGrammar :: Grammar
exprGrammar = Grammar prods e
  where
    e = NT "E"
    e' = NT "E'"
    t  = NT "T"
    t' = NT "T'"
    f  = NT "F"
    plus = T "+"
    times = T "*"
    lparen = T "("
    rparen = T ")"
    tid = T "id"
    prods = [ e  +-> [ Var t , Var e']
            , e' +-> [ Symb plus
                     , Var t
                     , Var e']
            , e' +-> [Symb Lambda]
            , t  +-> [Var f , Var t']
            , t' +-> [ Symb times
                     , Var f
                     , Var t']
            , t' +-> [ Symb Lambda ]
            , f  +-> [ Symb lparen
                     , Var e
                     , Symb rparen ]
            , f +->  [Symb tid ]
            ]

-- simple function for lexing input

exprLex :: String -> Maybe (String, String)
exprLex s
  = lex (elimSpaces s)
  where
    elimSpaces = concat . words
    wrap x = [x]
    isCharToken c = c `elem` "()+*$"
    lex [] = Just ([], [])
    lex x@(c : cs)
      | isCharToken c = Just (wrap c, cs)
      | take 2 x == "id" = Just ("id", drop 2 x)
      | otherwise = Nothing
      
