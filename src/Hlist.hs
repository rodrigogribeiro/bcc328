{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hlist where

import Data.Kind (Constraint, Type)
import Data.Type.Equality
import Data.Proxy
import GHC.TypeLits 


 -- definition of heterogeneous lists 

data HList (ts :: [(Symbol, Type)]) where 
    HNil :: HList '[]
    (:#) :: Pair s t -> HList ts -> HList ('(s, t) ': ts) 
infixr 5 :#

-- show instance for heterogeneous list 

type family ShowAll (ts :: [(Symbol, Type)]) :: Constraint where
    ShowAll '[] = () 
    ShowAll ( '(_ , t) ': ts) = (Show t, ShowAll ts)

instance ShowAll ts => Show (HList ts) where 
  show HNil = "[]"
  show (p :# ps) = show p ++ ":" ++ show ps

-- type level pair, for using with HList 

data Pair (s :: Symbol)(t :: Type) where 
  MkPair :: KnownSymbol s => Proxy s -> t -> Pair s t  

instance Show t => Show (Pair s t) where 
  show (MkPair p v) = "(" ++ symbolVal p ++ ", " ++ show v ++ ")"

-- lookup and update functions on HList 

hLookup :: In s t xs -> HList xs -> t 
hLookup Here ((MkPair _ v) :# _) = v 
hLookup (There p) (_ :# xs) = hLookup p xs

hUpdate :: In s t xs -> t -> HList xs -> HList xs 
hUpdate Here v ((MkPair s _) :# hs) = (MkPair s v) :# hs 
hUpdate (There p) v (h :# hs) = h :# (hUpdate p v hs)

-- typed De Bruijn indexes, we represent names in a contexts by its position 

data In (s :: Symbol)(t :: Type)(xs :: [(Symbol, Type)]) where 
  Here :: In s t ('(s , t) ': xs)
  There :: In s t xs -> In s t ( '(s1 , t1) ': xs)

-- type level conditional operator 

type family If (b :: Bool)(t :: k)(e :: k) :: k where 
  If 'True t _ = t 
  If 'False _ e = e 

-- type level function to looking up a symbol in a name context 

type family HasKey (g :: [(Symbol, Type)])(s :: Symbol) :: Maybe Type where 
  HasKey '[] _ = 'Nothing 
  HasKey ( '(s, a) ': g) s' = If (s == s') ('Just a) (HasKey g s')

-- a name is just a wrapper around the KnownSymbol contraint.
-- This is necessary to enforce that s will denote a constant. 

data Name (s :: Symbol) = KnownSymbol s => Name -- "abc"

instance Show (Name s) where 
  show Name = symbolVal (Proxy :: Proxy s)

-- Definition of a field value. 

data Field (s :: Symbol)(g :: [(Symbol,Type)]) (a :: Type) =
  HasField g s a (AtHead g '(s,a)) => The (Name s)

instance Show (Field s g a) where 
  show (The nm) = show nm 

-- type level function to check if a value is at first position of a list.

type family AtHead (g :: [k]) (v :: k) :: Bool where 
  AtHead '[] _ = 'False 
  AtHead (x ': xs) v = x == v

-- construction of the De Bruijn index using type class inference

class HasKey g s ~ 'Just a => HasField (g :: [(Symbol, Type)]) (s :: Symbol)(a :: Type)(b :: Bool) where 
  fieldPos' :: Proxy b -> Name s -> In s a g 

instance (s == s) ~ 'True => HasField ('(s, a) ': g) s a 'True where 
  fieldPos' _ _ = Here 

instance ((t == s) ~ 'False, HasField g s a (AtHead g '(s,a))) => HasField ('(t,b) ': g) s a 'False where 
  fieldPos' _ nm = There $ fieldPos nm 

-- main function to construct the index 

fieldPos :: forall g s a. HasField g s a (AtHead g '(s, a)) => Name s -> In s a g
fieldPos nm = fieldPos' (Proxy :: Proxy (AtHead g '(s, a))) nm

-- a record is just an heterogeneous list 

type Record g = HList g  

-- record lookup operation

fieldLookup :: forall g s a . Field s g a -> Record g -> a 
fieldLookup (The nm) rec = hLookup (fieldPos nm) rec


-- record update operation 

fieldUpdate :: forall g s a . Field s g a -> a -> Record g -> Record g 
fieldUpdate (The nm) v rec = hUpdate (fieldPos nm) v rec


-- val.x
-- "x" e HList 
example3 :: Record '[ '("nome", String), '("idade", Int)]
example3 = (MkPair (Proxy :: Proxy "nome") "Bruno") :# 
           (MkPair (Proxy :: Proxy "idade") 20) :# HNil -- Proxy :: (Proxy "nome", "Bruno") :# HNil


example4 :: Record '[ '("nome", String), '("idade", Int)]
example4 = fieldUpdate (The (Name :: Name "nome")) "Gabriel" example3

