module Exp.Frontend.Lexer.Token where 

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme 
    } deriving (Eq, Ord, Show)

data Lexeme    
  = TNumber Int
  | TLParen 
  | TRParen 
  | TPlus 
  | TTimes 
  deriving (Eq, Ord, Show)


