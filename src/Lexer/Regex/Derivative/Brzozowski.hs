module Lexer.Regex.Derivative.Brzozowski where

import Lexer.Regex.Syntax

-- definition of nullability test

nullable :: Regex a -> Bool
nullable Empty = False
nullable Lambda = True
nullable (Chr _) = False
nullable (Choice e1 e2)
  = nullable e1 || nullable e2
nullable (Cat e1 e2)
  = nullable e1 && nullable e2
nullable (Star _) = True

-- definition of derivatives

derivative :: Eq a => Regex a -> a -> Regex a
derivative Empty _ = Empty
derivative Lambda _ = Empty
derivative (Chr c) c'
  | c == c' = Lambda
  | otherwise = Empty
derivative (Choice e1 e2) c
  = derivative e1 c .+. derivative e2 c
derivative (Cat e1 e2) c
  | nullable e1 = (derivative e1 c .@. e2) .+. (derivative e2 c)
  | otherwise = derivative e1 c .@. e2
derivative (Star e1) c
  = derivative e1 c .@. (star e1)

-- smart constructors

(.+.) :: Regex a -> Regex a -> Regex a
Empty .+. e2 = e2
e1 .+. Empty = e1
e1 .+. e2 = Choice e1 e2

(.@.) :: Regex a -> Regex a -> Regex a
Empty .@. _ = Empty
_ .@. Empty = Empty
Lambda .@. e2 = e2
e1 .@. Lambda = e1
e1 .@. e2 = Cat e1 e2

star :: Regex a -> Regex a
star Empty = Lambda
star Lambda = Lambda
star (Star e1) = Star e1
star e1 = Star e1
