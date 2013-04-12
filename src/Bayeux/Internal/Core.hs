module Bayeux.Internal.Core where

--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------------------------------------------------------------------

nsendSync :: (Cloud.Serializable a, Cloud.Serializable b)
          => String -> a -> Cloud.Process b
nsendSync name msg = do
  (sp, rp) <- Cloud.newChan
  Cloud.nsend name (msg, sp)
  Cloud.receiveChan rp
