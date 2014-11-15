module Node where

import Types

nodeEquals :: Node -> Node -> Bool
nodeEquals x y = nodeId x == nodeId y
