{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.SmallCheck
import Test.SmallCheck.Series

newtype Light a = Light a

instance Serial m a => Serial m (Light a) where
  series = newtypeCons Light

data Tree a = Null | Fork (Tree a) a (Tree a)

instance Serial m a => Serial m (Tree a) where
  series = cons0 Null \/ cons3 Fork

instance CoSerial m a => CoSerial m (Tree a) where
  coseries rs =
    alts0 rs >>- \z ->
    alts3 rs >>- \f ->
    return $ \t ->
      case t of
        Null -> z
        Fork t1 x t2 -> f t1 x t2
  
instance CoSerial m a => CoSerial m (Light a) where
  coseries rs =
    newtypeAlts rs >>- \f ->
    return $ \l ->
      case l of
        Light x -> f x
