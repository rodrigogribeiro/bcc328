module Parser.LL.Predictive (predictive) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

import Parser.Common.Grammar
import Parser.LL.Table

type Lexer = String -> Maybe (String, String)

predictive :: Grammar -> String -> Lexer-> IO [Production]
predictive g s plex
  = do
       r <- fst <$> runPredictiveM (initial g s) (predictiveM plex)
       case r of
         (Left err, _) -> error err
         (Right _, ps) -> return ps

-- definition of the parsing state

data Pred
  = Pred {
      stack :: [Symbol]
    , input :: String
    , table :: Table
    }

initial :: Grammar -> String -> Pred
initial g s
  = Pred stk (s ++ "$") (buildTable g)
    where
      stk = [Var (start g), Symb Dollar]

-- definition of a monad for predictive parsing

type PredictiveM a = ExceptT String (WriterT [Production] (StateT Pred IO)) a

runPredictiveM :: Pred -> PredictiveM a -> IO ((Either String a, [Production]), Pred)
runPredictiveM st m
  = runStateT (runWriterT (runExceptT m)) st

emptyStack :: PredictiveM Bool
emptyStack = lift (null <$> gets stack)

pop :: PredictiveM Symbol
pop = do
  stk <- lift (gets stack)
  case stk of
    [] -> throwError "Unexpected empty stack!"
    (t : ts) -> do
        lift (modify (\ s -> s{ stack = ts}))
        liftIO $ putStrLn $ "Current stack:" ++ show ts
        return t

top :: PredictiveM Symbol
top = do
  stk <- lift (gets stack)
  case stk of
    [] -> throwError "Unexpected empty stack!"
    (t : _) ->
        return t

isTerminal :: Symbol -> Bool
isTerminal (Symb (T _)) = True
isTerminal (Symb Dollar) = True
isTerminal _ = False

isNonterminal :: Symbol -> Bool
isNonterminal (Var _) = True
isNonterminal _ = False

nextToken :: Lexer -> PredictiveM Terminal
nextToken plex
  = do
       inp <- lift (gets input)
       case plex inp of
         Nothing -> throwError "Lexer error."
         Just ("", _) -> throwError "Unexpected end of input!"
         Just (r , _) -> do
           return $ mkToken r

mkToken :: String -> Terminal
mkToken "$" = Dollar
mkToken s = T s

consumeToken :: Lexer -> PredictiveM ()
consumeToken plex
  = do
       inp <- lift (gets input)
       case plex inp of
         Nothing -> throwError "Lexer error."
         Just ("", _) -> throwError "Unexpected end of input!"
         Just (r , rs) -> do
           liftIO $ putStrLn $ "Consuming:" ++ r
           lift (modify (\s -> s {input = rs}))


push :: [Symbol] -> PredictiveM ()
push [Symb Lambda] = return ()
push ss = do
  liftIO $ putStrLn $ "Pushing " ++ show ss
  lift (modify (\ s -> s { stack = ss ++ stack s}))
  stk <- lift (gets stack)
  liftIO $ putStrLn $ "Current stack:" ++ show stk

expecting :: Symbol -> Terminal -> String
expecting s t
  = concat [ "Expecting "
           , show s
           , "\nFound:"
           , show t
           ]

lookupTable :: Nonterminal -> Terminal -> PredictiveM (Maybe Production)
lookupTable nt t
  = do
      liftIO $ putStrLn $ "Looking for M[" ++ show nt ++ "," ++ show t ++ "]"
      tbl <- lift (gets table)
      case Map.lookup (nt,t) tbl of
        Just (p : _) -> return (Just p)
        _            -> return Nothing

nonTerminal :: Symbol -> Nonterminal
nonTerminal (Var v) = v
nonTerminal _ = undefined

predictiveM :: Lexer -> PredictiveM ()
predictiveM plex
  = do
      v <- emptyStack
      if v then return ()
      else do
          r <- nextToken plex
          a <- top
          liftIO $ putStrLn $ "Top of the stack:" ++ show a
          liftIO $ putStrLn $ "Current token:" ++ show r
          when (isTerminal a && a /= (Symb r)) (throwError $ expecting a r)
          when (isTerminal a && a == (Symb r)) (pop >> consumeToken plex)
          when (isNonterminal a) $ do
            let nt' = nonTerminal a
            _ <- pop
            p <- lookupTable nt' r
            when (isNothing p) (throwError "Parsing table error!")
            let p' = fromJust p
            lift (tell [p'])
            push (rightHand p')
          predictiveM plex
