module Parser.Examples.IfElse ( ifElseGrammarOk
                              , ifElseGrammarNo
                              , ifElseLex
                              ) where

import Prelude hiding (lex)
import Parser.Common.Grammar

ifElseGrammarOk :: Grammar
ifElseGrammarOk = Grammar prods e
  where
    s = NT "S"
    e = NT "E"
    m = NT "M"
    o = NT "O"
    p = NT "P"
    c = T "c"
    b = T "b"
    tif = T "if"
    tthen = T "then"
    telse = T "else"
    prods = [
              s +-> [ Var m]
            , s +-> [ Var o]
            , m +-> [ Symb tif
                    , Var e
                    , Symb tthen 
                    , Var m 
                    , Symb telse 
                    , Var m 
                    ]
            , m +-> [ Symb c ]
            , o +-> [ Symb tif 
                    , Var e 
                    , Symb tthen 
                    , Var p 
                    ] 
            , p +-> [ Var s ] 
            , p +-> [ Var m 
                    , Symb telse 
                    , Var o 
                    ]
            , e +-> [Symb b]
            ]

ifElseGrammarNo :: Grammar
ifElseGrammarNo = Grammar prods e
  where
    s = NT "S"
    e = NT "E"
    p = NT "X"
    c = T "c"
    b = T "b"
    tif = T "if"
    tthen = T "then"
    telse = T "else"
    prods = [ s  +-> [ Symb tif 
                     , Var e 
                     , Symb tthen
                     , Var s
                     , Var p
                     ]
            , s +-> [Symb c]
            , e +-> [Symb b]
            , p +-> [ Symb telse, Var s]
            , p +-> [ Symb Lambda ]
            ]



-- simple function for lexing input

ifElseLex :: String -> Maybe (String, String)
ifElseLex s
  = lex (elimSpaces s)
  where
    elimSpaces = concat . words
    wrap x = [x]
    isCharToken c = c `elem` "bc"
    lex [] = Just ([], [])
    lex x@(c : cs)
      | isCharToken c = Just (wrap c, cs)
      | take 2 x == "if" = Just ("if", drop 2 x)
      | take 4 x == "then" = Just ("then", drop 4 x)
      | take 4 x == "else" = Just ("else", drop 4 x)
      | otherwise = Nothing
 
