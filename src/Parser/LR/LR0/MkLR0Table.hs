module Parser.LR.LR0.MkLR0Table where 

import Control.Monad.State 
import Control.Monad.Except 

import qualified Data.Map as Map 

import Parser.Common.Grammar

-- definition of the table 

type Table = Map.Map (Int, Symbol) (Action, Item)

-- type of Action 

data Action 
  = Shift Symbol 
  | Reduce Production 
  | Error 
  | Accept 
  deriving Eq 

instance Show Action where 
  show (Shift s) = "Shift: " ++ show s 
  show (Reduce r) = "Reduce: " ++ show r 
  show Error = "Error"
  show Accept = "Accept"

-- definition of an item 

type Item 
  = [Production]

-- definition of the monad 

data Env 
  = Env {
      fresh :: Int
    , states :: [Item]
    , table :: Table 
    }

type TableM a = StateT Env (ExceptT String IO) a

-- auxiliar stuff 

subset :: Eq a => [a] -> [a] -> Bool 
subset xs ys = all (\ x -> x `elem` ys) xs
