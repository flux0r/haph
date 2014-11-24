module Store.Class where

import Data.UUID

class Store a where
    nextId :: a -> UUID
    getTypeDescriptor :: a -> String
    getHighestPossibleIdInUse :: a -> UUID
    getNumberOfIdsInUse :: a -> Integer
