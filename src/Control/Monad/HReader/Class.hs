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


#if MIN_VERSION_base(4, 8, 0)
class (Monad m) => MonadHReader m where
#else
class (Monad m, Applicative m) => MonadHReader m where
#endif
  type HSetElements m :: [*]
  askHSet :: m (HSet (HSetElements m))

haskM :: (MonadHReader m, Contains (HSetElements m) e)
      => m e
haskM = hget <$> askHSet

haskLabeledM :: (MonadHReader m, Contains (HSetElements m) (Labeled label e))
             => proxy label -> m e
haskLabeledM p = hgetLabeled p <$> askHSet

instance (Monad m, MonadHReader m) => MonadHReader (ReaderT r m) where
  type HSetElements (ReaderT r m) = HSetElements m
  askHSet = lift askHSet
