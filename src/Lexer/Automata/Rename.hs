module Lexer.Automata.Rename where

import Control.Monad.State
import qualified Data.Map as Map

import Lexer.Automata.DFA


-- renaming

rename :: Ord a => (a -> c) -> DFA a b -> DFA c b
rename f m
  = DFA (f <$> dfaStates m)
        (dfaSigma m)
        (g <$> dfaTransition m)
        (f $ dfaStart m)
        (f <$> dfaFinals m)
    where
      g ((e,c),e') = ((f e, c), f e')


-- basic infra for renaming automata for regex.

data Ren a = Ren {
             table :: Map.Map a Int
           , next :: Int
           } deriving Show

rebuildStates :: Ord a => DFA a b -> DFA Int b
rebuildStates m = rename f m
                  where
                    tbl = buildRenameTable m
                    f es = case Map.lookup es tbl of
                              Nothing -> 0
                              Just n  -> n

ren :: Ren a
ren = Ren Map.empty 0

inc :: State (Ren a) Int
inc = do
  env <- get
  let n = next env
  put $ env {next = next env + 1}
  return (n + 1)

insertM :: Ord a => a -> Int -> State (Ren a) ()
insertM es n
  = modify (\ env -> env{table = Map.insert es n (table env)})

renameM :: Ord a => a -> State (Ren a) Int
renameM es
  = do
      env <- get
      let tbl = table env
          n = next env
      case Map.lookup es tbl of
        Nothing -> insertM es n >> inc
        Just n' -> return n'


buildRenameTable :: Ord a => DFA a b -> Map.Map a Int
buildRenameTable m
  = table $ execState (mapM_ renameM $ dfaStates m) ren
