module Parser.LL.Table (Table, buildTable, printTable,toTableString) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Parser.Common.First
import Parser.Common.Follow
import Parser.Common.Grammar


type Table = Map (Nonterminal, Terminal) [Production]

buildTable :: Grammar -> Table
buildTable g = foldr step Map.empty (productions g)
  where
    firstG = first g
    followG = follow g
    insertTable p s tbl = Map.insertWith union (leftHand p, s) [p] tbl 
    step p tbl = let 
                     firstM = Map.fromList firstG
                     lhs = leftHand p
                     rhs = rightHand p
                     firstP = firstForWord rhs firstM
                     followP = maybe [] id (lookup lhs followG)
                     tbl1 = foldr (insertTable p) tbl [x | x <- firstP, x /= Lambda]
                     tbl2 = if Lambda `elem` firstP
                            then foldr (insertTable p) tbl1 followP
                            else tbl1
                  in if Lambda `elem` firstP &&
                        Dollar `elem` followP
                     then insertTable p Dollar tbl2
                     else tbl2

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

printTable :: Grammar -> IO ()
printTable g
  = let table = buildTable g
    in putStrLn $ toTableString table
