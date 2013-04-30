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
                                                           Client (..),
                                                           ClientId,
                                                           ClientInbox,
                                                           ClientPid,
                                                           ClientStatus (..),
                                                           ClientStatusMap,
                                                           Context, clientId,
                                                           clientStatus)

--------------------------------------------------------------------------------

-- getMessagesFromClientInbox :: ClientId -> Cloud.Process [BayeuxInternalMsg]
-- getMessagesFromClientInbox clientId = do
--     liftIO $ putStrLn $ "[client.output" ++ clientId ++ "] sending"
--     sendToClientSync clientId ClientInboxRequest


--------------------------------------------------------------------------------

newClient :: MonadIO m => Context -> m Client
newClient ctx = liftIO . execContext ctx $ newClient'

newClient' :: Cloud.Process Client
newClient' = do
  cid <- sendHandshake
  client <- Client <$> (pure cid)
                   <*> (liftIO $ newTVarIO CONNECTING)
  spawnClient client
  return client

connectClient :: MonadIO m => Context -> Client -> m ()
connectClient ctx client = liftIO . execContext ctx $ connectClient' client

connectClient' :: Client -> Cloud.Process ()
connectClient' client = do
  sendToEngine [ConnectRequest (client ^. clientId)]


--------------------------------------------------------------------------------

updateClientStatus :: MonadIO m => Client -> ClientStatus -> m ()
updateClientStatus client cs = liftIO . atomically $ writeTVar (client ^. clientStatus) cs

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


handleMsg :: Client -> BayeuxInternalMsg -> Cloud.Process ()
handleMsg client (Response (ConnectRequest cid)) = do
    updateClientStatus client CONNECTED
handleMsg _ msg@(ErrorResponse _) = do
  error $ show msg ++ ": came back with an error"
handleMsg _ msg = error $ show msg ++ ": message not supported"
-- handleMsg clientInbox response = do
--     liftIO $ atomically (writeTChan clientInbox response)

--------------------------------------------------------------------------------

spawnClient :: Client -> Cloud.Process ()
spawnClient client = do
    -- clientInbox <- liftIO $ newClientInbox

    clientInputPid <- Cloud.spawnLocal $ forever $
         Cloud.receiveWait [
           Cloud.match (handleMsg client)
         , Cloud.match (mapM_ (handleMsg client))
         ]

    -- clientOutputPid <- Cloud.spawnLocal $ forever $
    --      Cloud.receiveWait [Cloud.match (handleInboxRequest clientInbox)]

    Cloud.register ("client.input." ++ (client ^. clientId)) clientInputPid
    -- Cloud.register ("client.output." ++ clientId) clientOutputPid
