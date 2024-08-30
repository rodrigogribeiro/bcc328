{
module IfExp.Frontend.Parser.Parser (ifExpParser) where

import IfExp.Frontend.Lexer.Lexer
import IfExp.Syntax.Syntax 
}


%name parser
%tokentype { Token }
%error     {parseError}


%token
      zero      {Token _ TZero}
      succ      {Token _ TSucc}
      pred      {Token _ TPred}
      true      {Token _ TTrue}
      false     {Token _ TFalse}
      if        {Token _ TIf}
      then      {Token _ TThen}
      else      {Token _ TElse}
      iszero    {Token _ TIsZero}
      '('       {Token _ TLParen}
      ')'       {Token _ TRParen}

%%

Exp : zero                                         {IZero}
    | true                                         {ITrue}
    | false                                        {IFalse}
    | succ Exp                                     {ISucc $2}
    | pred Exp                                     {IPred $2}
    | iszero Exp                                   {IIsZero $2}
    | if Exp then Exp else Exp                     {IIf $2 $4 $6}
    | '(' Exp ')'                                  {$2}

{
parseError :: [Token] -> a
parseError [] = error "Parse error!"
parseError (t : _) = error $ "Parse error " ++ (show t)


ifExpParser :: String -> IExp
ifExpParser = parser . lexer
}
