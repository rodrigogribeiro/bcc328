module Imp.Frontend.Parser.PEG.Parser where

import Parser.PEG.Grammar
import Imp.Syntax.Syntax


impParser :: String -> Either String Program
impParser s = runPEG programPEG s

programPEG :: PExp String Program
programPEG = Program <$> blockPEG

blockPEG :: PExp String Block
blockPEG = Block <$> braces (star stmtPEG)

stmtPEG :: PExp String Stmt
stmtPEG 
  = choice [ skipPEG
           , readPEG 
           , printPEG 
           , defVarPEG
           , assignPEG
           , ifPEG
           , whilePEG
           ]

skipPEG :: PExp String Stmt
skipPEG 
  = f <$> token "skip" <*> semi
    where 
      f _ _ = Skip 

readPEG :: PExp String Stmt
readPEG 
  = f <$> token "read" <*> varPEG <*> semi
    where 
      f _ v _ = SRead v

printPEG :: PExp String Stmt
printPEG 
  = f <$> token "print" <*> (parens expPEG) <*> semi
    where 
      f _ e _ = Print e

defVarPEG :: PExp String Stmt
defVarPEG 
  = f <$> tyPEG <*> varPEG <*> initPEG <*> semi
    where 
      f t v mi _ = Def t v mi

tyPEG :: PExp String Ty
tyPEG = choice [ TInt <$ token "int"
               , TBool <$ token "bool"
               ]

varPEG :: PExp String Var
varPEG = Var <$> identifier

identifier :: PExp String String
identifier = (:) <$> letter <*> star (letter </> digit)

initPEG :: PExp String (Maybe Exp) 
initPEG 
  = (f <$> token ":=" <*> expPEG) </> lambda Nothing
    where 
      f _ e = Just e 

assignPEG :: PExp String Stmt
assignPEG 
  = f <$> varPEG <*> expPEG <*> semi
    where 
      f v e _ = v := e

ifPEG :: PExp String Stmt
ifPEG 
  = f <$> token "if"   <*> 
          expPEG       <*> 
          token "then" <*> 
          blockPEG     <*> 
          token "else" <*> 
          blockPEG
    where 
      f _ e _ tb _ eb = If e tb eb

whilePEG :: PExp String Stmt
whilePEG 
  = f <$> token "while" <*> 
          expPEG        <*> 
          blockPEG
    where 
      f _ e b = While e b

expPEG :: PExp String Exp
expPEG = undefined
