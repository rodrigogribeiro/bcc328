module SVM.StackLexer ( Parser
                      , sc
                      , lexeme
                      , symbol
                      , parens
                      , brackets
                      , rword
                      , int
                      , letter
                      , alphaNum
                      ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L


type Parser = Parsec Void String

-- skipping white space and comments

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "//"
  blockCmnt = L.skipBlockComment "/*" "*/"


-- defining simple parsers to be used in lexical analysis

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = lexeme . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

letter :: Parser Char
letter = letterChar

alphaNum :: Parser Char
alphaNum = alphaNumChar

-- definition of reserved words

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- integer constants

int :: Parser Int
int = L.signed sc (lexeme L.decimal)
