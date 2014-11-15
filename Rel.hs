module Rel where

import Types
import Data.UUID

relEquals :: Rel -> Rel -> Bool
relEquals x y = relId x == relId y

relShow :: Rel -> String
relShow x = "Rel #" ++ show (relId x) ++
    " of type " ++ show (relType x) ++
    " between Node[" ++ show (relStartNodeId x) ++
    "] and Node[" ++ show (relEndNodeId x) ++
    "]"

relStartNodeId :: Rel -> UUID
relStartNodeId = nodeId . startNode

relEndNodeId :: Rel -> UUID
relEndNodeId = nodeId . endNode

instance Eq Rel where
    (==) = relEquals

instance Show Rel where
    show = relShow
