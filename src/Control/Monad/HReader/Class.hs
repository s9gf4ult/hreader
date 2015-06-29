module Control.Monad.HReader.Class where

import Data.HSet
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif



class HasHSet m where
  type HSetElements m :: [*]
  askHSet :: m (HSet (HSetElements m))

instance (Monad m, HasHSet m) => HasHSet (ReaderT r m) where
  type HSetElements (ReaderT r m) = HSetElements m
  askHSet = lift askHSet
