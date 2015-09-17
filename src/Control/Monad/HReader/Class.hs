module Control.Monad.HReader.Class
       ( MonadHReader(..)
       , MHRElemsConstraint
       , hask
       , haskTagged
       ) where


import Control.Monad.Cont
import Control.Monad.List
import Control.Monad.Reader
import Data.HSet
import Data.Tagged
import GHC.Exts

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#endif

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Monoid
#endif

import qualified Control.Monad.RWS.Lazy       as RWSL
import qualified Control.Monad.RWS.Strict     as RWSS
import qualified Control.Monad.State.Lazy     as SL
import qualified Control.Monad.State.Strict   as SS
import qualified Control.Monad.Writer.Lazy    as WL
import qualified Control.Monad.Writer.Strict  as WS

-- | Easy generate constraint like
-- __(HGettable (MHRElements m) Int, HGettable (MHRElements m) Bool)__
-- from type list __[Int, Bool]__. Usable to reuse type lists for
-- constraints and concrete HSet.
type family MHRElemsConstraint (m :: * -> *) (els :: [*]) :: Constraint where
  MHRElemsConstraint m '[] = ()
  MHRElemsConstraint m (e ': els) = (HGettable (MHRElements m) e, MHRElemsConstraint m els)

-- | Monad which is a reader of HSet (or just can construct it).
class (Monad m, Applicative m) => MonadHReader m where
  type MHRElements m :: [*]
  askHSet :: m (HSet (MHRElements m))

-- | Ask arbitrary element of hset inside HReader
hask :: (MonadHReader m, HGettable (MHRElements m) e)
      => m e
hask = hget <$> askHSet

-- | Ask arbitrary labeled element of hset in HReader
haskTagged :: (MonadHReader m, HGettable (MHRElements m) (Tagged tag e))
           => proxy tag
           -> m e
haskTagged p = hgetTagged p <$> askHSet

#define MHR(MONAD)                                        \
instance (MonadHReader m) => MonadHReader (MONAD) where { \
  type MHRElements (MONAD) = MHRElements m ;            \
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
  type MHRElements (WL.WriterT w m) = MHRElements m
  askHSet = lift askHSet

instance (MonadHReader m, Monoid w) => MonadHReader (WS.WriterT w m) where
  type MHRElements (WS.WriterT w m) = MHRElements m
  askHSet = lift askHSet

instance (MonadHReader m, Monoid w) => MonadHReader (RWSL.RWST r w s m) where
  type MHRElements (RWSL.RWST r w s m) = MHRElements m
  askHSet = lift askHSet

instance (MonadHReader m, Monoid w) => MonadHReader (RWSS.RWST r w s m) where
  type MHRElements (RWSS.RWST r w s m) = MHRElements m
  askHSet = lift askHSet
