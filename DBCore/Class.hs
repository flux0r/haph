module Haph.DBCore.Class where

import Data.UUID

class HasSize a where
    size :: a -> Int

class (HasSize a) => EntityWithSize a where
    entityId :: a -> UUID
