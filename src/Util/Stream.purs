module Util.Stream (
    readStreamToString
    ) where


import Prelude
import Control.Monad.Aff
import Control.Monad.Aff.Unsafe
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Unsafe
import Control.Monad.ST
import Node.Stream

readStreamToString :: forall w eff. Readable w eff String -> Aff eff String
readStreamToString stream = unsafeInterleaveAff do
    buf <- liftEff $ newSTRef ""
    makeAff \_ success -> unsafeInterleaveEff do
        onData stream \d -> unsafeInterleaveEff do
            modifySTRef buf (\a -> d ++ a)
            return unit
        onEnd stream $ unsafeInterleaveEff do
            body <- readSTRef buf
            success body
