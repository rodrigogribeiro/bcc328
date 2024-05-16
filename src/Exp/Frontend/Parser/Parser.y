{
module Exp.Frontend.Parser.Parser (expParser) where

import Exp.Frontend.Lexer.Lexer
import Exp.Syntax.Syntax
}


%name parser
%tokentype { Token }
%error     {parseError}


%token
      num       {Token _ (TNumber $$)}
      '('       {Token _ TLParen}
      ')'       {Token _ TRParen}
      '+'       {Token _ TPlus}
      '*'       {Token _ TTimes}

%left '+'
%left '*'

%%

Exp : num                                          {EInt $1}
    | Exp '+' Exp                                  {$1 :+: $3}
    | Exp '*' Exp                                  {$1 :*: $3}
    | '(' Exp ')'                                  { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error!"
parseError (t : _) = error $ "Parse error " ++ (show t)


expParser :: String -> Exp
expParser = parser . lexer
}
