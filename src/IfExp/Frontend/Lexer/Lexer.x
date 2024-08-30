
{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module IfExp.Frontend.Lexer.Lexer (Token (..), Lexeme (..), lexer) where

import IfExp.Frontend.Lexer.Token 
}


%wrapper "posn"


-- tokens declarations

tokens :-
      $white+       ;
      "//" .*       ;
      "zero"       {simpleToken TZero}
      "("          {simpleToken TLParen}
      ")"          {simpleToken TRParen}
      "succ"       {simpleToken TSucc}
      "pred"       {simpleToken TPred}
      "iszero"     {simpleToken TIsZero}
      "if"         {simpleToken TIf}
      "then"       {simpleToken TThen}
      "else"       {simpleToken TElse}
      "true"       {simpleToken TTrue}
      "false"      {simpleToken TFalse}

{
position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}
