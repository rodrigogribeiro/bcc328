module Parser.PEG.Grammar ( module Parser.PEG.ParsingExp
                          , token
                          , runPEG
                          , parens
                          , braces
                          , semi
                          , letter 
                          , digit
                          , choice
                          ) where 

import Control.Applicative
import Data.Char 
import Parser.PEG.ParsingExp

-- running a PEG 

runPEG :: PExp s a -> s -> Either String a 
runPEG p inp =
  case runPExp p inp of
    Pure r -> Right r 
    Commit _ r -> Right r  
    Fail s _ -> Left s

choice :: [PExp s a] -> PExp s a 
choice = foldr (</>) empty

-- function for parsing a token, 
-- discarding whitespace 

token :: String -> PExp String String 
token s 
  = do
      whitespace
      _ <- mapM symbol s 
      whitespace
      pure s

whitespace :: PExp String ()
whitespace = () <$ star (satisfy isSpace)

-- packing stuff 

pack :: PExp s a -> PExp s b -> PExp s a -> PExp s b 
pack o r c 
  = f <$> o <*> r <*> c 
    where 
      f _ x _ = x

parens :: PExp String a -> PExp String a 
parens p 
  = pack (token "(") p (token ")")

braces :: PExp String a -> PExp String a 
braces p 
  = pack (token "{") p (token "}")

-- basic stuff 

semi :: PExp String ()
semi = () <$ token ";"

letter :: PExp String Char 
letter = satisfy isLetter

digit :: PExp String Char 
digit = satisfy isDigit

