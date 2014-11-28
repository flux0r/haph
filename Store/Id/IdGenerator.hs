module Store.Id.IdGenerator where

import qualified Data.ByteString.Char8 as B

import Control.Concurrent.STM
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromJust)
import System.FilePath (FilePath)
import System.IO (SeekMode(AbsoluteSeek), Handle, hSeek)

data IdGenerator = IdGenerator
    { grabSize          :: Int
    , highId            :: TVar Integer
    , readPosition      :: TVar Integer
    , maxReadPosition   :: Integer
    , defraggedIdCount  :: TVar Integer
    , fileName          :: FilePath
    , fileHandle        :: Maybe Handle
    , idsReadFromFile   :: TQueue Integer
    , releasedIdList    :: TQueue Integer
    , max               :: Integer
    , aggressiveReuse   :: Bool
    }

headerSize :: Int
headerSize = 9

data IdError = ClosedIdGenerator String
             | ExceededCapacity

stillOpen :: IdGenerator -> Bool
stillOpen x = case fileHandle x of
    Nothing     -> False
    _           -> True

canReadMoreIdBatches :: IdGenerator -> STM Bool
canReadMoreIdBatches x = do
    pos <- readTVar (readPosition x)
    return (pos < toInteger headerSize)

aggressiveIdFromDefragList :: IdGenerator -> IO (Maybe Integer)
aggressiveIdFromDefragList x =
    atomically (tryReadTQueue (releasedIdList x)) >>= decrement
  where
    decrement (Just ident) = do
        atomically (modifyTVar' (defraggedIdCount x) (\n -> n - 1))
        return (Just ident)
    decrement _            = return Nothing

-- TODO: This needs to be written more Haskelly.
readIdBatch :: IdGenerator -> IO (Maybe IdGenerator)
readIdBatch x = do
        can <- atomically (canReadMoreIdBatches x)
        if not can
            then return Nothing
            else do
                let h = fromJust (fileHandle x)
                pos <- atomically (readTVar (readPosition x))
                let n = findN pos
                hSeek h AbsoluteSeek pos
                bytes <- B.hGet h n
                atomically (update (toInteger n))
                addIds bytes
                return (Just x)
  where
    update count = do
        modifyTVar' (readPosition x) (\old -> old + count)
        modifyTVar' (defraggedIdCount x) (\old -> old - count)
    findN p = fromInteger (min (toInteger ((grabSize x)*8)) (maxReadPosition x - p))
    addIds ids = iter (B.readInteger ids)
      where
        iter (Just (i, xs)) = do
            when (i /= -1) (atomically (writeTQueue (idsReadFromFile x) i))
            if B.null xs
                then return ()
                else iter (B.readInteger xs)
        -- TODO: SHould this be an error?
        iter Nothing = return ()
