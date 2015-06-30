module Control.Monad.HReader
       ( HReaderT(..)
       , runHReaderT
       , module Control.Monad.HReader.Class
       ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.HReader.Class
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Data.HSet
import Data.Typeable
import GHC.Generics

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif


newtype HReaderT els m a = HReaderT
    { unHReaderT :: ReaderT (HSet els) m a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadError e, MonadCont, MonadWriter w
               , MonadState s, MonadBase b
               , MonadThrow, MonadCatch
               , Typeable, Generic  )

runHReaderT :: HSet els -> HReaderT els m a -> m a
runHReaderT h (HReaderT r) = runReaderT r h

instance MonadTrans (HReaderT els) where
  lift = HReaderT . lift

instance (MonadReader r m) => MonadReader r (HReaderT els m) where
  ask = lift ask
  local f ma = HReaderT $ do
    h <- ask
    lift $ do
      local f $ runHReaderT h ma

instance (Monad m) => MonadHReader (HReaderT els m) where
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
instance MonadTransControl (HReaderT els) where
  newtype StT (HReaderT els) a
    = HRtTT
      { unHRtTT :: StT (ReaderT (HSet els)) a
      }
  liftWith action = HReaderT $ do
    liftWith $ \runTrans -> do
      action ((HRtTT `liftM`) . runTrans . unHReaderT)
  restoreT st = HReaderT $ restoreT $ unHRtTT `liftM` st

instance (MonadBaseControl b m) => MonadBaseControl b (HReaderT els m) where
  newtype StM (HReaderT els m) a
    = HRtMT (StM (ReaderT (HSet els) m) a)
  liftBaseWith action = HReaderT $ do
    liftBaseWith $ \runInBase -> do
      action ((HRtMT `liftM`) . runInBase . unHReaderT)
  restoreM (HRtMT st) = HReaderT $ restoreM st
#endif
