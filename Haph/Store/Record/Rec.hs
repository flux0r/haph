module Haph.Store.Record.Rec where

import Haph.Store.Record.Types

setInUse :: Rec -> Rec
setInUse (Rec _ x y) = Rec True x y

setCreated :: Rec -> Rec
setCreated (Rec x _ y) = Rec x True y
