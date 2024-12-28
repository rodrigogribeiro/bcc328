module Parser.PEG.Examples where 

import Data.Char

import Parser.PEG.ParsingExp
import Prelude hiding (and, not)


-- examples

manya :: PExp String String
manya = ((:) <$> symbol 'a' <*> manya) </> lambda ""

ab :: PExp String String
ab = (f <$> symbol 'a' <*> ab <*> symbol 'b') </> lambda ""
  where
    f x s y = x : s ++ [y]

bc :: PExp String String
bc = (f <$> symbol 'b' <*> bc <*> symbol 'c') </>
     lambda ""
  where
    f x s y = x : s ++ [y]


ab1 :: PExp String String
ab1 = (f <$> symbol 'a' <*> ab <*> symbol 'b') </>
      (g <$> symbol 'a' <*> symbol 'b')
  where
    f x s y = x : s ++ [y]
    g x y = [x,y]

bc1 :: PExp String String
bc1 = (f <$> symbol 'b' <*> bc <*> symbol 'c') </>
      (g <$> symbol 'b' <*> symbol 'c')
  where
    f x s y = x : s ++ [y]
    g x y = [x,y]

abc :: PExp String String
abc = f <$> and (ab *> not b) <*>
            star a            <*>
            bc                <*>
            not anyChar
  where
    a = symbol 'a'
    b = symbol 'b'
    f _ as bcs _ = as ++ bcs

abc1 :: PExp String String
abc1 = (f <$> and (ab1 *> not b) <*>
             star a             <*>
             bc1                <*>
             not anyChar) </> ("" <$ not anyChar)
  where
    a = symbol 'a'
    b = symbol 'b'
    f _ as bcs _ = as ++ bcs

parens :: PExp String String
parens = (f <$> lparen <*> parens <*> rparen <*> parens) </> lambda ""
  where
    f a b c d = a : b ++ [c] ++ d
    lparen = symbol '('
    rparen = symbol ')'


expr :: PExp String String
expr = f <$> term <*> star ((++) <$> plus <*> expr) 
  where 
     f s1 ss = s1 ++ concat ss
     plus = (wrap <$> symbol '+') </> (wrap <$> symbol '-')

wrap :: a -> [a]
wrap x = [x]

term :: PExp String String 
term = f <$> factor <*> star ((++) <$> times <*> term)
      where 
        f s1 ss = s1 ++ concat ss 
        times = (wrap <$> symbol '*') </> (wrap <$> symbol '/')

factor :: PExp String String 
factor = (f <$> symbol '(' <*> expr <*> symbol ')') </> number </> var
   where
      f c s c' = c : s ++ [c']

number :: PExp String String 
number = (:) <$> digit <*> star (satisfy isDigit)

digit :: PExp String Char 
digit = satisfy isDigit

var :: PExp String String 
var = f <$> letter <*> star (letter </> digit)
      where 
        letter = satisfy isLetter
        f l s = l : s
