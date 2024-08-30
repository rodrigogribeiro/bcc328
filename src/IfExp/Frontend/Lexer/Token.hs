
module IfExp.Frontend.Lexer.Token where 

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme 
    } deriving (Eq, Ord, Show)

data Lexeme    
  = TZero 
  | TSucc 
  | TIsZero 
  | TPred 
  | TTrue 
  | TFalse 
  | TIf 
  | TThen 
  | TElse 
  | TLParen 
  | TRParen 
  deriving (Eq, Ord, Show)


