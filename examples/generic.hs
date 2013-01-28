{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
import Test.SmallCheck.Series
import GHC.Generics

data Tree a = Null | Fork (Tree a) a (Tree a)
    deriving Generic
instance Serial m a => Serial m (Tree a)
