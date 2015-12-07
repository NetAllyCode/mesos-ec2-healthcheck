module Node.AWS where

import Control.Monad.Eff
import Control.Monad.ST

foreign import data Config :: *

foreign import awsConfig :: forall eff. Eff (st :: ST Config | eff) Config
