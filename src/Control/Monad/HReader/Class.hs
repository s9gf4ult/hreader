module Control.Monad.HReader.Class where

import Control.Monad.Cont
import Control.Monad.List
import Control.Monad.Reader
import Data.HSet

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

import qualified Control.Monad.RWS.Lazy   as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.State.Lazy   as SL
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.Writer.Lazy   as WL
import qualified Control.Monad.Writer.Strict as WS


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

#define MHR(MONAD)                                \
instance (MonadHReader m) => MonadHReader (MONAD) where { \
  type HSetElements (MONAD) = HSetElements m ;            \
  askHSet = lift askHSet ;                                \
  }

MHR(ReaderT r m)
MHR(ContT r m)
MHR(ListT m)

#if MIN_VERSION_mtl(2, 2, 1)
MHR(ExceptT e m)
#endif

MHR(SL.StateT s m)
MHR(SS.StateT s m)

instance (MonadHReader m, Monoid w) => MonadHReader (WL.WriterT w m) where
  type HSetElements (WL.WriterT w m) = HSetElements m
  askHSet = lift askHSet

instance (MonadHReader m, Monoid w) => MonadHReader (WS.WriterT w m) where
  type HSetElements (WS.WriterT w m) = HSetElements m
  askHSet = lift askHSet

instance (MonadHReader m, Monoid w) => MonadHReader (RWSL.RWST r w s m) where
  type HSetElements (RWSL.RWST r w s m) = HSetElements m
  askHSet = lift askHSet

instance (MonadHReader m, Monoid w) => MonadHReader (RWSS.RWST r w s m) where
  type HSetElements (RWSS.RWST r w s m) = HSetElements m
  askHSet = lift askHSet