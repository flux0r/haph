module Props where

import Types
import Data.Text
import qualified Data.Map as M

prop :: Text -> Props -> Maybe PropVal
prop k ps = M.lookup k (props ps)
 
propDefault :: PropVal -> Text -> Props -> PropVal
propDefault v k ps = M.findWithDefault v k (props ps)

propModify :: Props -> Text -> PropVal -> Props
propModify ps k v = Props (graph ps) (M.insert k v (props ps))

propRm :: Text -> Props -> Props
propRm k ps = Props (graph ps) (M.delete k (props ps))

propNames :: Props -> [Text]
propNames ps = M.keys (props ps)
