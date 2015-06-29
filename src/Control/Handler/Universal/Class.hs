module Control.Handler.Universal.Class where

import Data.HSet


class HasHSet m where
  type HSetElements m :: [*]
  askHSet :: m (HSet (HSetElements m))
