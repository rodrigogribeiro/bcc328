{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Imp.Frontend.Lexer.Lexer (Token (..), Lexeme (..), lexer) where
}


%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

-- second RE macros

@identifier = $alpha[$alpha $digit]* -- identifiers
@number     = $digit+


-- tokens declarations

tokens :-
      $white+       ;
      "//" .*       ;
      @number       {mkNumber}
      ":="          {simpleToken TAssign}
      "read"        {simpleToken TRead}
      "print"       {simpleToken TPrint}
      "if"          {simpleToken TIf}
      "then"        {simpleToken TThen}
      "else"        {simpleToken TElse}
      "while"       {simpleToken TWhile}
      ";"           {simpleToken TSemi}
      "("           {simpleToken TLParen}
      ")"           {simpleToken TRParen}
      "{"           {simpleToken TLBrace}
      "}"           {simpleToken TRBrace}
      "+"           {simpleToken TPlus}
      "*"           {simpleToken TTimes}
      "-"           {simpleToken TMinus}
      "/"           {simpleToken TDiv}
      "=="          {simpleToken TEq}
      "<"           {simpleToken TLt}
      "!"           {simpleToken TNot}
      "&&"          {simpleToken TAnd}
      "int"         {simpleToken TTInt}
      "bool"        {simpleToken TTBool}
      "true"        {simpleToken TTrue}
      "false"       {simpleToken TFalse}
      "skip"        {simpleToken TSkip}
      @identifier    {mkIdent}

{

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme 
    } deriving (Eq, Ord, Show)

data Lexeme    
  = TIdent String
  | TNumber Int
  | TAssign 
  | TRead 
  | TPrint 
  | TIf 
  | TThen 
  | TElse 
  | TWhile 
  | TSemi 
  | TLParen 
  | TRParen 
  | TLBrace 
  | TRBrace 
  | TPlus 
  | TTimes 
  | TMinus 
  | TDiv 
  | TEq 
  | TLt 
  | TNot 
  | TAnd 
  | TTInt 
  | TTBool 
  | TTrue 
  | TFalse 
  | TSkip 
  deriving (Eq, Ord, Show)

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkIdent :: AlexPosn -> String -> Token 
mkIdent p s
  | s == "skip" = Token (position p) TSkip
  | s == "if"   = Token (position p) TIf
  | s == "then" = Token (position p) TThen
  | s == "else" = Token (position p) TElse
  | s == "true" = Token (position p) TTrue
  | s == "false" = Token (position p) TFalse
  | s == "int"   = Token (position p) TTInt
  | s == "bool"  = Token (position p) TTBool
  | s == "read"  = Token (position p) TRead
  | s == "print" = Token (position p) TPrint
  | s == "while" = Token (position p) TWhile
  | otherwise = Token (position p) (TIdent s)

mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

lexer :: String -> [Token]
lexer = alexScanTokens
}
