{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable
import Data.UUID

data G = G
    { schema        :: Schema
    , index         :: Index
    , traversal     :: Traversal
    , BiTraversal   :: BiTraversal
    }

data Direction = Out
               | In
               | Both

newtype RelType = RelType { unRelType :: Text }

newtype Lab = Lab { unLab :: Text }

data Rel = Rel
    { relId     :: UUID
    , relProps  :: Props
    , relType   :: RelType
    , startNode :: Node
    , endNode   :: Node
    }


data Node = Node
    { nodeId        :: UUID
    , nodeProps     :: Props
    , relations     :: [Rel]
    , nodeDegree    :: Int64
    , labels        :: [Lab]
    }

data PropVal = PropText Text
             | PropByteString ByteString
             | PropInt Int64
             | PropDouble Double
             | PropRational Rational
             | PropBool Bool
             | PropDay Day
             | PropTimeOfDay TimeOfDay
             | PropUtc UTCTime
             | PropNull
             | PropList [PropVal]
             | PropMap (M.Map Text PropVal)
             | PropExt ByteString
  deriving (Show, Read, Eq, Typeable, Ord)

data Props = Props { graph   :: G
                   , props   :: M.Map Text PropVal
                   }

data IndexState = Online
                | Populating

data Schema =
    { schemaIndexState  :: IndexState
    , schemaIndexes     :: [IndexDefinition]
    , schemaConstraints :: [ConstraintDefinition]
    }
