module Haph.Store.Record.Types where

import Data.ByteString (ByteString)
import Data.Word (Word8)

data RecordField = RecordField
    { byteValue :: Word8
    , intValue  :: Int
    }

data RecordType = NotInUse
                | InUse
                | FirstInChain
                | Reserved
                | NoNextProperty
                | NoPreviousProperty
                | NoNextRelationship
                | NoPrevRelationship
                | NotDirected
                | Directed
                | NoNextBlock
                | NoPrevBlock
                | NodeProperty
                | RelProperty
                | NoLabelsField

data Record = Record
    { recordType    :: RecordType
    , recordField   :: RecordField
    }

data Rec = Rec
    { inUse     :: Bool
    , created   :: Bool
    , longId    :: Integer
    }

data DynRec = DynRec
    { dynRecRecord      :: Record
    , dynRecRec         :: Rec
    , dynRecData        :: ByteString
    , dynRecNextBlock   :: Int
    , dynRecType        :: Int
    , dynRecStartRecord :: Bool
    }
