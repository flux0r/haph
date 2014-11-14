{-# LANGUAGE DeriveDataTypeable #-}

module Types where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable
import Data.UUID

data Traversal
data BiTraversal

data G = G
    { schema        :: Schema
    , index         :: IndexDefinition
    , traversal     :: Traversal
    , biTraversal   :: BiTraversal
    }

data Direction = Out
               | In
               | Both

newtype RelType = RelType { unRelType :: Text }

newtype Lab = Lab { unLab :: Text }

data ConstraintType = Uniqueness

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

data T = TNode | TRel
    deriving (Show)

data PropEntry = PropEntry
    { propEntryEntityType       :: T
    , propEntryKey              :: Text
    , propEntryPrevVal          :: PropVal
    , propEntryVal              :: PropVal
    }

data IndexState = Online
                | Populating

data Schema = Schema
    { schemaIndexState  :: IndexState
    , schemaIndexes     :: [IndexDefinition]
    , schemaConstraints :: [ConstraintDefinition]
    }

data IndexDefinition = IndexDefinition
    { indexLabel        :: Lab
    , isConstraintIndex :: Bool
    }

data ConstraintDefinition = ConstraintDefinition
    { constraintLabel       :: Lab
    , constraintPropKeys    :: [Text]
    , constraintType        :: ConstraintType
    }
