{-# LANGUAGE DeriveDataTypeable #-}

module GraphTraverse where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable
import Data.UUID

data G
data Lab

newtype RelType = RelType { unRelType :: Text }

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

prop :: Text -> Props -> Maybe PropVal
prop k ps = M.lookup k (props ps)
 
propDefault :: PropVal -> Text -> Props -> PropVal
propDefault v k ps = M.findWithDefault v k (props ps)

propModify :: Props -> Text -> PropVal -> Props
propModify ps k v = Props (graph ps) (M.insert k v (props ps))

propRm :: Text -> Props -> Props
propRm k ps = Props (graph ps) (M.delete k (props ps))

propNames :: Props -> [Text]
propNames ps = M.keys (props ps)
