module Bayeux.Internal.Client where

--------------------

import           Control.Applicative                      (pure, (<$>), (<*>))
import           Control.Lens                             (at, (^.))
import           Control.Monad                            (forever)
import           Control.Monad.Trans                      (MonadIO (..))

--------------------

import           Control.Concurrent.STM                   (atomically,
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
                                                           ClientId,
                                                           ClientInbox,
                                                           ClientPid,
                                                           ClientState (..),
                                                           ClientStatus (..),
                                                           ClientStatusMap,
                                                           Context,
                                                           clientStateId,
                                                           clientStateStatus)

--------------------------------------------------------------------------------

-- getMessagesFromClientInbox :: ClientId -> Cloud.Process [BayeuxInternalMsg]
-- getMessagesFromClientInbox clientStateId = do
--     liftIO $ putStrLn $ "[client.output" ++ clientStateId ++ "] sending"
--     sendToClientSync clientStateId ClientInboxRequest


--------------------------------------------------------------------------------

newClientState :: MonadIO m => Context -> m ClientState
newClientState ctx = liftIO . execContext ctx $ newClientState'

newClientState' :: Cloud.Process ClientState
newClientState' = do
  cid <- sendHandshake
  client <- ClientState <$> (pure cid)
                        <*> (liftIO $ newTVarIO CONNECTING)
  spawnClient client
  return client

connectClient :: MonadIO m => Context -> ClientState -> m ()
connectClient ctx client = liftIO . execContext ctx $ connectClient' client

connectClient' :: ClientState -> Cloud.Process ()
connectClient' client = do
  sendToEngine [ConnectRequest (client ^. clientStateId)]


--------------------------------------------------------------------------------

updateClientStatus :: MonadIO m => ClientState -> ClientStatus -> m ()
updateClientStatus client cs = liftIO . atomically $ writeTVar (client ^. clientStateStatus) cs

--------------------------------------------------------------------------------

newClientInbox :: IO ClientInbox
newClientInbox = newTChanIO

--------------------------------------------------------------------------------

-- handleInboxRequest :: ClientInbox
--                    -> (BayeuxInternalMsg, Cloud.SendPort [BayeuxInternalMsg])
--                    -> Cloud.Process ()
-- handleInboxRequest clientInbox (ClientInboxRequest, sPort) = do
--     msgs <- liftIO . atomically $ do
--               msg <- readTChan clientInbox
--               readInboxContents [msg]
--     Cloud.sendChan sPort msgs
--   where
--     readInboxContents acc = do
--       shouldStop <- isEmptyTChan clientInbox
--       if shouldStop
--          then return (reverse acc)
--          else do
--            msg <- readTChan clientInbox
--            readInboxContents (msg:acc)
-- handleInboxRequest _ _ = error "Sending invalid sync message to client"


handleMsg :: ClientState -> BayeuxInternalMsg -> Cloud.Process ()
handleMsg client (Response (ConnectRequest cid)) = do
    updateClientStatus client CONNECTED
handleMsg _ msg@(ErrorResponse _) = do
  error $ show msg ++ ": came back with an error"
handleMsg _ msg = error $ show msg ++ ": message not supported"
-- handleMsg clientInbox response = do
--     liftIO $ atomically (writeTChan clientInbox response)

--------------------------------------------------------------------------------

spawnClient :: ClientState -> Cloud.Process ()
spawnClient client = do
    -- clientInbox <- liftIO $ newClientInbox

    clientInputPid <- Cloud.spawnLocal $ forever $
         Cloud.receiveWait [
           Cloud.match (handleMsg client)
         , Cloud.match (mapM_ (handleMsg client))
         ]

    -- clientOutputPid <- Cloud.spawnLocal $ forever $
    --      Cloud.receiveWait [Cloud.match (handleInboxRequest clientInbox)]

    Cloud.register ("client.input." ++ (client ^. clientStateId)) clientInputPid
    -- Cloud.register ("client.output." ++ clientStateId) clientOutputPid
