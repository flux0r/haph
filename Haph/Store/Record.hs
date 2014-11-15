module Haph.Store.Record where

import Haph.Store.Record.Types

recordIs :: Record -> Int -> Bool
recordIs (Record _ fld) n = intValue fld == n
