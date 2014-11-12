import Prop
import Data.UUID
import Data.Int (Int64)

data Rel
data Lab

data Node = Node
    { nodeId        :: UUID
    , nodeProps     :: Props
    , relations     :: [Rel]
    , nodeDegree    :: Int64
    , labels        :: [Lab]
    }
