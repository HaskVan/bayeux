module Bayeux.Internal.Engine where

--------------------

import qualified Data.HashMap.Strict                      as HashMap

--------------------

import           Control.Applicative                      ((<$>), (<*>))
import           Control.Concurrent.STM                   (TVar, atomically,
                                                           modifyTVar,
                                                           newTVarIO)
import           Control.Lens                             (at, (&), (.~), (^.))
import           Control.Monad                            (forever)
import           Control.Monad.Trans                      (MonadIO (..))

--------------------

import           Test.QuickCheck                          (resize, sample')
import           Test.QuickCheck.Gen                      (Gen (..), elements,
                                                           listOf1)

--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------

import           Bayeux.Internal.Client                   (checkClientIsConnected,
                                                           sendToClient)
import           Bayeux.Internal.Core                     (nsendSync)
import           Bayeux.Internal.Types                    (BayeuxInternalMsg (..),
                                                           ClientId,
                                                           ClientStatus (..),
                                                           ClientStatusMap, ClientSubscriptionMap,
                                                           EngineState (..), engineClientStatusMap)

--------------------------------------------------------------------------------

actorName :: String
actorName = "bayeux.engine"

--------------------------------------------------------------------------------

sendToEngine :: Cloud.Serializable a => a -> Cloud.Process ()
sendToEngine = Cloud.nsend actorName

sendToEngineSync :: (Cloud.Serializable a, Cloud.Serializable b)
                 => a -> Cloud.Process b
sendToEngineSync = nsendSync actorName

sendHandshake :: Cloud.Process String
sendHandshake = do
  liftIO $ putStrLn "[engine] Sending Handshake request"
  response <- sendToEngineSync HandshakeRequest
  case response of
    HandshakeResponse clientId -> do
      liftIO . putStrLn $ "[engine] Received HandshakeResponse's clientId: " ++ clientId
      return clientId
    _ -> error "You got a bug on engine's Handshake handler"

--------------------------------------------------------------------------------

clientIdChars :: String
clientIdChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genClientId :: IO String
genClientId = fmap head
                   (sample' .
                    resize 30 .
                    listOf1 $
                    elements clientIdChars)

newClientStatusMap :: IO ClientStatusMap
newClientStatusMap = newTVarIO HashMap.empty

newClientSubscriptionMap :: IO ClientSubscriptionMap
newClientSubscriptionMap = newTVarIO HashMap.empty

newEngineState :: IO EngineState
newEngineState =
    EngineState <$> newClientStatusMap
                <*> newClientSubscriptionMap

updateClientStatus :: ClientStatusMap -> ClientId -> ClientStatus -> IO ()
updateClientStatus csmRef clientId clientSt =
    atomically $ modifyTVar csmRef updateClientStatus'
  where
    updateClientStatus' csm = csm & at clientId .~ (Just clientSt)

--------------------------------------------------------------------------------

handleSyncBayeuxMsg :: (BayeuxInternalMsg, Cloud.SendPort BayeuxInternalMsg)
                    -> Cloud.Process ()
handleSyncBayeuxMsg (Ping, sPort) = Cloud.sendChan sPort Pong
handleSyncBayeuxMsg (HandshakeRequest, sPort) = do
    liftIO $ putStrLn "[engine] Handling Handshake request"
    clientId <- liftIO genClientId
    Cloud.sendChan sPort $ HandshakeResponse clientId
handleSyncBayeuxMsg _ = return ()

handleBayeuxMsg :: EngineState -> BayeuxInternalMsg -> Cloud.Process ()
handleBayeuxMsg es (ConnectRequest clientId) = do
    liftIO $ updateClientStatus (es ^. engineClientStatusMap)
                                clientId CONNECTED
    sendToClient clientId ConnectResponse
handleBayeuxMsg es msg@(SubscribeRequest clientId chanName) = do
    isClientConnected <- liftIO $ checkClientIsConnected (es ^. engineClientStatusMap)
                                                         clientId
    if not isClientConnected
       then sendToClient clientId (ErrorResponse msg)
       else return ()
handleBayeuxMsg _ _ = error "not supported yet"

--------------------------------------------------------------------------------

spawnEngine :: Cloud.Process ()
spawnEngine = do
    es <- liftIO $ newEngineState
    enginePid <- Cloud.spawnLocal $ forever $
      Cloud.receiveWait [
        Cloud.match handleSyncBayeuxMsg
      , Cloud.match (mapM_ (handleBayeuxMsg es))]
    Cloud.register "bayeux.engine" enginePid
    return ()
