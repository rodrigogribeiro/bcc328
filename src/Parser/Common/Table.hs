module Parser.Common.Table ( Table
                           , toTableString
                           ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Parser.Common.Grammar


type Table = Map (Nonterminal, Terminal) [Production]

toTableString :: Table -> String
toTableString tbl
  = concatMap format (Map.toList tbl)
    where
      format ((nt, t), p) = concat [ "M["
                                   , show nt
                                   , ", "
                                   , show t
                                   , "] = "
                                   , show p
                                   , "\n"
                                   ]


