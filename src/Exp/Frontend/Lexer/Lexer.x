{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Exp.Frontend.Lexer.Lexer (Token (..), Lexeme (..), lexer) where

import Exp.Frontend.Lexer.Token 
}


%wrapper "posn"

$digit = 0-9            -- digits

-- second RE macros

@number     = $digit+


-- tokens declarations

tokens :-
      $white+       ;
      "//" .*       ;
      @number       {mkNumber}
      "("           {simpleToken TLParen}
      ")"           {simpleToken TRParen}
      "+"           {simpleToken TPlus}
      "*"           {simpleToken TTimes}

{
position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}
