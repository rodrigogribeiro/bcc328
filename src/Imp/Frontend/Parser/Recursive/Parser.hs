module Imp.Frontend.Parser.Recursive.Parser where 

{-
 - Recursive descendent parser 
 - This is an scannerless parser. 
 - -}

import Parser.Recursive.SimpleCombinators
import Imp.Syntax.Syntax

-- top level parsing function 

impParser :: String -> Either String Program
impParser s 
  = case runParser programParser s of 
      [] -> Left "Parser error"
      ((r,_) : _) -> Right r

-- main program parser 

programParser :: Parser Char Program
programParser 
  = Program <$> blockParser

-- parser for statements 
 
stmtParser :: Parser Char Stmt 
stmtParser 
  = choice [
             skipParser
           , varDeclParser  
           , assignParser
           , ifParser 
           , printParser 
           , readParser 
           ]

-- skip statement 

skipParser :: Parser Char Stmt 
skipParser 
  = Skip <$ (stringToken "skip") <* (stringToken ";")

-- variable declaration 

varDeclParser :: Parser Char Stmt
varDeclParser
  = f <$> tyParser <*> varParser <*> initParser <*> semi 
    where 
      f t v i _ = Def t v i

-- variable 

varParser :: Parser Char Var 
varParser = Var <$> identifier

-- type 

tyParser :: Parser Char Ty 
tyParser = intParser <|> tboolParser 

intParser :: Parser Char Ty 
intParser = TInt <$ (stringToken "int")

tboolParser :: Parser Char Ty 
tboolParser = TBool <$ (stringToken "bool")

-- initialization 

initParser :: Parser Char (Maybe Exp)
initParser = (Just <$> rhsParser) <|> pure Nothing 

rhsParser :: Parser Char Exp
rhsParser 
  = f <$> stringToken ":=" <*> expParser 
    where
      f _ e = e

-- assignment 

assignParser :: Parser Char Stmt 
assignParser 
  = f <$> varParser <*> rhsParser <*> semi 
    where 
      f v e _ = v := e


-- blocks 

blockParser :: Parser Char Block
blockParser 
  = Block <$> braces (greedy stmtParser)

-- if statement 

ifParser :: Parser Char Stmt
ifParser 
  = f <$> stringToken "if"   <*> 
          expParser          <*> 
          stringToken "then" <*> 
          blockParser        <*> 
          stringToken "else" <*> 
          blockParser
    where 
      f _ b _ bt _ be = If b bt be

-- print statement 

printParser :: Parser Char Stmt 
printParser 
  = Print <$> expParser 

-- read statement 

readParser :: Parser Char Stmt 
readParser 
  = SRead <$> varParser

-- while statement 

whileParser :: Parser Char Stmt 
whileParser 
  = f <$> stringToken "while" <*> expParser <*> blockParser
    where 
      f _ e b = While e b

-- values 

valueParser :: Parser Char Value 
valueParser = vintParser <|> vboolParser 

vintParser :: Parser Char Value
vintParser = EInt <$> integer 

vboolParser :: Parser Char Value 
vboolParser = EBool <$> boolParser 
  where 
    boolParser = trueParser <|> falseParser 
    trueParser = True <$ stringToken "true"
    falseParser = False <$ stringToken "false"

-- expressions.

expParser :: Parser Char Exp 
expParser = andExpParser

andExpParser :: Parser Char Exp
andExpParser 
  = chainl relExpParser andop 
    where 
      andop = (:&:) <$ stringToken "&&"

relExpParser :: Parser Char Exp
relExpParser 
  = chainr plusExpParser relop  
    where 
      relop = eqp <|> ltp
      eqp = (:==:) <$ stringToken "=="
      ltp = (:<:) <$ stringToken "<"

plusExpParser :: Parser Char Exp
plusExpParser 
  = chainl mulExpParser plusop  
    where 
      plusop = plusp <|> minusp
      plusp = (:+:) <$ stringToken "+"
      minusp = (:-:) <$ stringToken "-"

mulExpParser :: Parser Char Exp
mulExpParser 
  = chainl notExpParser mulop  
    where 
      mulop = timesp <|> divp
      timesp = (:*:) <$ stringToken "*"
      divp = (:/:) <$ stringToken "/"

notExpParser :: Parser Char Exp
notExpParser 
  = (const ENot <$> (stringToken "!")) `option` id <*> factorParser 

factorParser :: Parser Char Exp
factorParser 
  = choice [ EValue <$> valueParser
           , EVar <$> varParser
           , parens expParser 
           ]
    
