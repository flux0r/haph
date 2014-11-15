module Haph.Store.Class where

import Data.Text
import Data.UUID

class IdSequence a => Store a where
    typeDescriptor          :: a -> Text 
    highestPossibleIdInUse  :: a -> UUID
    numberOfIdsInUse        :: a -> Integer

class IdSequence a where
    nextId  :: a -> UUID
