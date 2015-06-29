module Control.Handler.Universal where

import Control.Handler.Universal.Class
import Control.Monad.Base
import Control.Monad.Cont
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Data.HSet

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

newtype HReaderT els m a = HReaderT
    { unHReaderT :: ReaderT (HSet els) m a
    } deriving ( Functor, Applicative, Monad
               , MonadError e, MonadCont, MonadWriter w
               , MonadState s, MonadBase b )

runUnversalT :: HSet els -> HReaderT els m a -> m a
runUnversalT h (HReaderT r) = runReaderT r h

instance MonadTrans (HReaderT els) where
  lift = HReaderT . lift

instance (MonadReader r m) => MonadReader r (HReaderT els m) where
  ask = lift ask
  local f ma = HReaderT $ do
    h <- ask
    lift $ do
      local f $ runUnversalT h ma

instance (Monad m) => HasHSet (HReaderT els m) where
  type HSetElements (HReaderT els m) = els
  askHSet = HReaderT ask

deriving instance MFunctor (HReaderT els)
deriving instance MMonad (HReaderT els)

#if MIN_VERSION_monad_control(1, 0, 0)
instance MonadTransControl (HReaderT els) where
  type StT (HReaderT els) a = StT (ReaderT (HSet els)) a
  liftWith action = HReaderT $ do
    liftWith $ \runTrans -> action (runTrans . unHReaderT)
  restoreT = HReaderT . restoreT

instance (MonadBaseControl b m) => MonadBaseControl b (HReaderT els m) where
  type StM (HReaderT els m) a = StM (ReaderT (HSet els) m) a
  liftBaseWith action = HReaderT $ do
    liftBaseWith $ \runInBase -> action (runInBase . unHReaderT)
  restoreM = HReaderT . restoreM
#else

#endif
