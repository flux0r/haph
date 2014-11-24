module Store.Id.Class where

import Data.UUID

class IdSequence a where
    nextId :: a -> UUID
