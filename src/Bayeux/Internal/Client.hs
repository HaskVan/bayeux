module Bayeux.Internal.Client where


import           Data.ByteString                          (ByteString)

--------------------

import           Control.Applicative                      (pure, (<$>), (<*>))
import           Control.Lens                             (at, (^.))
import           Control.Monad                            (forever)
import           Control.Monad.Trans                      (MonadIO (..))

--------------------

import           Control.Concurrent.STM                   (STM, atomically,
                                                           newTVarIO, readTVar,
                                                           writeTVar)
import           Control.Concurrent.STM.TChan             (isEmptyTChan,
                                                           newTChanIO,
                                                           readTChan,
                                                           writeTChan)

--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------

import           Bayeux.Internal.Context                  (execContext)
import           Bayeux.Internal.Core                     (nsendSync)
import           Bayeux.Internal.Messages                 (sendHandshake,
                                                           sendToEngine)
import           Bayeux.Internal.Types                    (BayeuxInternalMsg (..),
                                                           ChanName, ClientId,
                                                           ClientInbox,
                                                           ClientPid,
                                                           ClientState (..),
                                                           ClientStatus (..),
                                                           ClientStatusMap,
                                                           Context,
                                                           clientStateId,
                                                           clientStateInbox,
                                                           clientStateStatus)

--------------------------------------------------------------------------------

newClientState :: MonadIO m => Context -> m ClientState
newClientState ctx = liftIO . execContext ctx $ newClientState'

newClientState' :: Cloud.Process ClientState
newClientState' = do
  cid <- sendHandshake
  client <- ClientState <$> (pure cid)
                        <*> (liftIO $ newTVarIO CONNECTING)
                        <*> (liftIO $ newTChanIO)
  spawnClient client
  return client

connectClient :: MonadIO m => Context -> ClientState -> m ()
connectClient ctx client = liftIO . execContext ctx $ connectClient' client

connectClient' :: ClientState -> Cloud.Process ()
connectClient' client = sendToEngine [ConnectRequest (client ^. clientStateId)]

sendToEngine' :: ClientState -> [BayeuxInternalMsg] -> Cloud.Process ()
sendToEngine' client msgs = do
    status <- liftIO $ atomically $ readTVar (client ^. clientStateStatus)
    case status of
      CONNECTED -> sendToEngine msgs
      _ -> sendToEngine (ConnectRequest cid : msgs)
  where
    cid = client ^. clientStateId

subscribe' :: ClientState -> ChanName -> Cloud.Process ()
subscribe' client chanName =
    sendToEngine' client [SubscribeRequest (client ^. clientStateId) chanName]

publish' :: ClientState -> ChanName -> ByteString -> Cloud.Process ()
publish' client chanName payload =
    sendToEngine' client [PublishRequest (client ^. clientStateId) chanName payload]

--------------------------------------------------------------------------------

updateClientStatus :: MonadIO m => ClientState -> ClientStatus -> m ()
updateClientStatus client cs = liftIO . atomically $ writeTVar (client ^. clientStateStatus) cs

appendToClientInbox :: MonadIO m => ClientState -> BayeuxInternalMsg -> m ()
appendToClientInbox client msg = liftIO . atomically $ writeTChan (client ^. clientStateInbox) msg

readInboxContents :: MonadIO m => ClientState -> m [BayeuxInternalMsg]
readInboxContents client = liftIO . atomically $ readInboxContentsSTM client

readInboxContentsSTM :: ClientState -> STM [BayeuxInternalMsg]
readInboxContentsSTM client = do
    msg <- readTChan clientInbox
    loop [msg]
  where
    clientInbox = client ^. clientStateInbox
    loop acc = do
      shouldStop <- isEmptyTChan clientInbox
      if shouldStop
         then return (reverse acc)
         else do
           msg <- readTChan clientInbox
           loop (msg:acc)

--------------------------------------------------------------------------------

handleInboxRequest :: ClientState
                   -> (BayeuxInternalMsg, Cloud.SendPort [BayeuxInternalMsg])
                   -> Cloud.Process ()
handleInboxRequest client (ClientInboxRequest, sPort) = do
    msgs <- liftIO $ readInboxContents client
    Cloud.sendChan sPort msgs
handleInboxRequest _ _ = error "Sending invalid sync message to client"


handleMsg :: ClientState -> BayeuxInternalMsg -> Cloud.Process ()
handleMsg client msg@(Response (ConnectRequest {})) = do
  updateClientStatus client CONNECTED
  appendToClientInbox client msg
handleMsg client msg@(Response {}) = appendToClientInbox client msg
handleMsg client msg@(PublishRequest {}) = appendToClientInbox client msg
handleMsg _ msg@(ErrorResponse _) = error $ show msg ++ ": came back with an error"
handleMsg _ msg = error $ show msg ++ ": message not supported"
-- handleMsg clientInbox response = do
--     liftIO $ atomically (writeTChan clientInbox response)

--------------------------------------------------------------------------------

spawnClient :: ClientState -> Cloud.Process ()
spawnClient client = do

    clientInputPid <- Cloud.spawnLocal $ forever $
         Cloud.receiveWait [
           Cloud.match (handleMsg client)
         , Cloud.match (mapM_ (handleMsg client))
         ]

    clientOutputPid <- Cloud.spawnLocal $ forever $
         Cloud.receiveWait [Cloud.match (handleInboxRequest client)]

    Cloud.register ("client.input." ++ (client ^. clientStateId)) clientInputPid
    Cloud.register ("client.output." ++ (client ^. clientStateId)) clientOutputPid
