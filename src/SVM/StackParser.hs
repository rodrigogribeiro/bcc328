module SVM.StackParser (parseCode) where

import Control.Monad.Combinators
import Text.Megaparsec

import SVM.StackLexer
import SVM.Instr

-- parser for instructions

parseCode :: String -> Either String [Instr]
parseCode s
  = case parse codeParser "" s of
      Left err -> Left $ errorBundlePretty err
      Right code -> Right code

codeParser :: Parser [Instr]
codeParser = (many instrParser) <* eof

instrParser :: Parser Instr
instrParser
  = parseSimpleInstr <|>
    parseParamInstr

parseSimpleInstr :: Parser Instr
parseSimpleInstr
  = choice $ map f opList
    where
      f (s, r) = r <$ (try $ symbol s)

parseParamInstr :: Parser Instr
parseParamInstr
  = choice $ map f pList
    where
      f (s, r) = r <$> try (symbol s *> int)

-- basic instructions

opList :: [(String, Instr)]
opList = [ ("noop", NOOP)
         , ("load", LOAD)
         , ("store", STORE)
         , ("pushpc", PUSHPC)
         , ("poppc", POPPC)
         , ("in", IN)
         , ("out", OUT)
         , ("add", ADD)
         , ("sub", SUB)
         , ("mul", MUL)
         , ("div", DIV)
         , ("shl", SHL)
         , ("shr", SHR)
         , ("band", BAND)
         , ("bor", BOR)
         , ("bxor", BXOR)
         , ("and", AND)
         , ("or", OR)
         , ("ieq", IEQ)
         , ("ne", NE)
         , ("ge", GE)
         , ("le", LE)
         , ("igt", IGT)
         , ("ilt", ILT)
         , ("bnot", BNOT)
         , ("not", NOT)
         , ("halt", HALT)
         ]

-- instructions with parameters

pList :: [(String, Int -> Instr)]
pList = [ ("pushi", PUSHI)
        , ("push", PUSH)
        , ("pop", POP)
        , ("jmp", JMP)
        , ("jz", JZ)
        , ("jnz", JNZ)
        ] 

