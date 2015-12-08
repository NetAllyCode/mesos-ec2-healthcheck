module Node.AWS where

import Control.Monad.Eff
import Control.Monad.ST

-- | Foreign type for AWS configuration data
foreign import data Config :: *

-- | Returns the contents of the native `AWS.config` dictionary
foreign import awsConfig :: forall eff. Eff (st :: ST Config | eff) Config
