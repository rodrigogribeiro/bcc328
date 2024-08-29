module Parser.Common.ExtendGrammar where 

import Data.List ((\\))
import Parser.Common.Grammar 

-- creating the extended grammar by adding 
-- a new production

extendGrammar :: Grammar -> Grammar 
extendGrammar g = Grammar rules' n' 
  where 
    n' = freshNT g 
    rules' = [ n' +-> [Symb Dot, Var (start g), Symb Dollar ]] ++ 
             productions g

-- creating a list of fresh non-terminals

freshNT :: Grammar -> Nonterminal 
freshNT g = NT v 
  where 
    (v, _) = deplete (ntPool \\ ns) 
    ns = ntName <$> nonterminals g 

ntName :: Nonterminal -> String 
ntName (NT s) = s

deplete :: [String] -> (String, [String])
deplete (x : xs) = (x, xs)
deplete [] = ("", [])

ntPool :: [String]
ntPool 
  = names ++ addNumber names ([1..] :: [Int])
  where 
    wrap x = [x]
    names = map wrap ['A' .. 'Z']
    addNumber :: [String] -> [Int] -> [String]
    addNumber as bs = do 
      a <- as 
      b <- bs 
      return $ a ++ show b

