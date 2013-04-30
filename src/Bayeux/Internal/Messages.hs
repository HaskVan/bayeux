module Bayeux.Internal.Messages where

--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------

import           Bayeux.Internal.Core                     (nsendSync)
import           Bayeux.Internal.Types (ClientId)

--------------------------------------------------------------------------------


sendToClient :: (Cloud.Serializable a) => ClientId -> a -> Cloud.Process ()
sendToClient clientId = Cloud.nsend ("client.input." ++  clientId)

sendToClientSync :: (Cloud.Serializable a, Cloud.Serializable b)
                 => ClientId -> a -> Cloud.Process b
sendToClientSync clientId = nsendSync ("client.output." ++ clientId)
