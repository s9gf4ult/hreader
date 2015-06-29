module Control.Handler.Universal where

import Control.Handler.Universal.Class
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.HSet

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

newtype UniversalT els m a = UniversalT
    { unUniversal :: ReaderT (HSet els) m a
    } deriving ( Functor, Applicative, Monad
               , MonadError e, MonadCont, MonadWriter w
               , MonadState s )

instance MonadTrans (UniversalT els) where
  lift = UniversalT . lift

instance (MonadReader r m) => MonadReader r (UniversalT els m) where
  ask = lift ask
  local f ma = UniversalT $ do
    h <- ask
    lift $ do
      local f $ runUnversalT h ma

instance (Monad m) => HasHSet (UniversalT els m) where
  type HSetElements (UniversalT els m) = els
  askHSet = UniversalT ask

deriving instance (HasHSet m) => HasHSet (ReaderT r m)

runUnversalT :: HSet els -> UniversalT els m a -> m a
runUnversalT h (UniversalT r) = runReaderT r h
