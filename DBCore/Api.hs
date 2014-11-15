{-# LANGUAGE DeriveDataTypeable #-}

module Haph.DBCore.Api where

import qualified Data.Map as M

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Typeable
import Data.UUID (UUID)

data Prop = Prop
    { propKeyId :: UUID
    , propVal   :: PropVal
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
