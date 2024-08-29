module IfExp.Syntax where 

data IExp 
  = IZero 
  | ISucc IExp 
  | IIsZero IExp 
  | IPred IExp 
  | ITrue 
  | IFalse 
  | IIf IExp IExp IExp 
  deriving (Eq, Show)

interp :: IExp -> Result Value 
interp IZero = pure (Right NZero)
interp ITrue = pure (Left BTrue)
interp IFalse = pure (Left BFalse)
interp (ISucc e) 
  = do 
      v <- interp e 
      case v of 
        Right nv -> pure (Right (NSucc nv))
        _        -> TypeError 
interp (IPred e)
  = do 
      v <- interp e 
      case v of 
        Left _ -> TypeError 
        Right NZero -> pure (Right NZero)
        Right (NSucc nv) -> pure (Right nv)
interp (IIsZero e)
  = do 
      v <- interp e 
      case v of 
        Left _ -> TypeError 
        Right NZero -> pure (Left BTrue)
        Right (NSucc _) -> pure (Left BFalse)
interp (IIf e1 e2 e3) 
  = do 
      v1 <- interp e1 
      case v1 of 
        Right _ -> TypeError 
        Left BTrue -> interp e2 
        Left BFalse -> interp e3 


type Value = Either BValue NValue 

data BValue 
  = BTrue 
  | BFalse 
  deriving (Eq, Show)

data NValue 
  = NZero 
  | NSucc NValue 
  deriving (Eq, Show)

data Result a 
  = Ok a 
  | TypeError 
  deriving (Eq, Show) 

instance Functor Result where 
  fmap f (Ok v) = Ok (f v)
  fmap _ TypeError = TypeError 

instance Applicative Result where 
  pure = Ok 
  (Ok f) <*> (Ok x) = Ok (f x)
  TypeError <*> _ = TypeError 
  _ <*> TypeError = TypeError

instance Monad Result where 
  return = pure 
  (Ok m) >>= f = f m 
  TypeError >>= _ = TypeError 


