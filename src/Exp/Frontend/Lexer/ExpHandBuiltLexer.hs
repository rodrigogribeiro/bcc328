module Exp.Frontend.Lexer.ExpHandBuiltLexer where 

import Exp.Frontend.Lexer.Token
import Data.Char

lexer :: String -> Maybe [Token]
lexer = mk . foldl step (Just (1,1,[],[]))
  where
    step Nothing _ = Nothing
    step (Just (l,col,t,ts)) c
      | c == '\n' = Just (l + 1, col, t, ts)
      | isSpace c = Just (l, col + 1, "", mkDigits l col t ++ ts)
      | isOp c    = Just (l, col + 1, "", mkToken l col c t ++ ts)
      | isSep c   = Just (l, col + 1, "", mkToken l col c t ++ ts) 
      | isDigit c = Just (l, col + 1, c:t, ts)
      | otherwise = Nothing 
    isOp c = c `elem` "+*" 
    isSep c = c `elem` "()"
    
    mkToken l col c t 
        = let ys = mkDigits l col t 
              y = mkT l col c 
          in y : ys

    mkDigits l col t 
      | not $ null t && all isDigit t 
          = let n = length t 
            in [Token (l, (col - n)) (TNumber $ read t)]
      | otherwise = []

    mkT l col c = Token (l, col) (case c of 
                                '*' -> TTimes
                                '+' -> TPlus
                                '(' -> TLParen
                                ')' -> TRParen
                                _ -> error "impossible!")
        
    mk Nothing = Nothing 
    mk (Just (l, col, t, ts))
      | all isDigit t = Just (reverse $ mkDigits l col t ++ ts)
      | otherwise = Nothing


{-
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v [] = v
foldl f v (x : xs) = foldl f (f v x) xs

-}
