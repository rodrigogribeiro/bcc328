module SVM.Instr ( Instr (..)
                 , pprintInstr
                 , pprintCode
                 , pprintCodeD
                 , Code
                 ) where

import Data.Char (toLower)

-- definition of the instruction syntax

type Code = [Instr]

data Instr
  = NOOP          -- do nothing instruction
  | PUSHI Int     -- push immediate value
  | PUSH Int      -- push value from memory (parameter: address)
  | POP Int       -- pop value from stack and store it on the address
  | JMP Int       -- change pc to specific value
  | JZ Int        -- change pc if top of the stack is equal to zero
  | JNZ Int       -- change pc if top of the stack is different from zero
  | LOAD          -- pushs the value at the memory address of top of the stack
  | STORE         -- stores the top of the stack in the address of the next stack value
  | PUSHPC        -- push the pc on the stack
  | POPPC         -- changes the pc to stack top
  | IN            -- reads an int from standard input and push it
  | OUT           -- prints the top of the stack
  | ADD           -- push next + top
  | SUB           -- push next - top
  | MUL           -- push next * top
  | DIV           -- push next / top
  | SHL           -- push next << top
  | SHR           -- push next >> top
  | BAND          -- push next & top
  | BOR           -- push next | top
  | BXOR          -- push next xor top
  | AND           -- push next && top
  | OR            -- push next || top
  | IEQ           -- push next == top
  | NE            -- push next != top
  | GE            -- push next >= top
  | LE            -- push next <= top
  | IGT           -- push next > top
  | ILT           -- push next < top
  | NEG           -- push - top
  | BNOT          -- push ! top (bitwise)
  | NOT           -- push ! top (boolean)
  | HALT          -- stop the machine
  deriving (Eq, Show)

pprintCode :: [Instr] -> String
pprintCode = unlines . map pprintInstr

pprintInstr :: Instr -> String
pprintInstr
  = filter f . map toLower . show
    where
      f c = c  `notElem` "()"

pprintCodeD :: [Instr] -> String
pprintCodeD = unlines . map f . zip ([0..] :: [Int])
  where
    f (k, i) = show k ++ " " ++ pprintInstr i
