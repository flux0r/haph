module Store.Id.Types where

import Data.UUID

data IdRange = IdRange
    { defragIds     :: [UUID]
    , rangeStart    :: Integer
    , rangeLength   :: Int
    }

instance Show IdRange where
    show (IdRange defrags start len) = "IdRange[" ++ show (start + toInteger len - 1) ++
                                       ", defrag " ++ show defrags ++ "]"
