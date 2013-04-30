module Bayeux.Internal.Messages where

import           Control.Monad.Trans                      (MonadIO (..))
--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------

import           Bayeux.Internal.Core                     (nsendSync)
import           Bayeux.Internal.Types                    (BayeuxInternalMsg (..),
                                                           ClientId)

-- Engine Messages -------------------------------------------------------------

sendToEngine :: Cloud.Serializable a => a -> Cloud.Process ()
sendToEngine = Cloud.nsend "bayeux.engine"

sendToEngineSync :: (Cloud.Serializable a, Cloud.Serializable b)
                 => a -> Cloud.Process b
sendToEngineSync = nsendSync "bayeux.engine"

sendHandshake :: Cloud.Process String
sendHandshake = do
  liftIO $ putStrLn "[engine] Sending Handshake request"
  response <- sendToEngineSync HandshakeRequest
  case response of
    HandshakeResponse clientId -> do
      liftIO . putStrLn $ "[engine] Received HandshakeResponse's clientId: " ++ clientId
      return clientId
    msg -> error $ "You got a bug on engine's Handshake handler, recieved: " ++ (show msg)

-- Client Messages -------------------------------------------------------------

sendToClient :: (Cloud.Serializable a) => ClientId -> a -> Cloud.Process ()
sendToClient clientId = Cloud.nsend ("client.input." ++  clientId)

sendToClientSync :: (Cloud.Serializable a, Cloud.Serializable b)
                 => ClientId -> a -> Cloud.Process b
sendToClientSync clientId = nsendSync ("client.output." ++ clientId)
