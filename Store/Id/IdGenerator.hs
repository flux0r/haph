module Store.Id.IdGenerator where

import Control.Concurrent.STM
import System.FilePath (FilePath)
import System.IO (Handle)

data IdGenerator = IdGenerator
    { grabSize          :: Int
    , highId            :: TVar Integer
    , readPosition      :: Integer
    , defraggedIdCount  :: TVar Integer
    , fileName          :: FilePath
    , fileHandle        :: Maybe Handle
    , idsReadFromFile   :: TQueue Integer
    , releasedIdList    :: TQueue Integer
    , max               :: Integer
    , aggressiveReuse   :: Bool
    }

data IdError = ClosedIdGenerator String
             | ExceededCapacity

stillOpen :: IdGenerator -> Bool
stillOpen x = case fileHandle x of
    Nothing     -> False
    _           -> True

aggressive :: IdGenerator -> IO (Maybe Integer)
aggressive x = (atomically . tryReadTQueue . releasedIdList $ x) >>= decrement
  where
    decrement (Just ident) = do
        atomically (modifyTVar' (defraggedIdCount x) (\n -> n - 1))
        return (Just ident)
    decrement _            = return Nothing
