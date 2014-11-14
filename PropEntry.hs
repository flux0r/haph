module PropEntry where

import Types

showPropEntry :: PropEntry -> String
showPropEntry x =
    "PropEntry[" ++ show (propEntryEntityType x) ++
    ", key:" ++ show (propEntryKey x) ++
    ", val:" ++ show (propEntryVal x) ++
    ", prevVal:" ++ show (propEntryPrevVal x) ++
    "]"

instance Show PropEntry where
    show = showPropEntry
