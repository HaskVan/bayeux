module Bayeux.Internal.Client where

--------------------

import           Control.Concurrent.STM                   (atomically, readTVar)
import           Control.Concurrent.STM.TChan             (isEmptyTChan,
                                                           newTChanIO,
                                                           readTChan,
                                                           writeTChan)
import           Control.Lens                             (at, (^.))
import           Control.Monad                            (forever)
import           Control.Monad.Trans                      (MonadIO (..))

--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------

import           Bayeux.Internal.Core                     (nsendSync)
import           Bayeux.Internal.Types                    (BayeuxInternalMsg (..),
                                                           ClientId,
                                                           ClientInbox,
                                                           ClientPid,
                                                           ClientStatus (..),
                                                           ClientStatusMap)

--------------------------------------------------------------------------------

sendToClient :: (Cloud.Serializable a) => ClientId -> a -> Cloud.Process ()
sendToClient clientId = Cloud.nsend ("client.input." ++  clientId)

sendToClientSync :: (Cloud.Serializable a, Cloud.Serializable b)
                 => ClientId -> a -> Cloud.Process b
sendToClientSync clientId = nsendSync ("client.output." ++ clientId)

getMessagesFromClientInbox :: ClientId -> Cloud.Process [BayeuxInternalMsg]
getMessagesFromClientInbox clientId = do
    liftIO $ putStrLn $ "[client.output" ++ clientId ++ "] sending"
    sendToClientSync clientId ClientInboxRequest


--------------------------------------------------------------------------------

newClientInbox :: IO ClientInbox
newClientInbox = newTChanIO

checkClientIsConnected :: ClientStatusMap -> ClientId -> IO Bool
checkClientIsConnected csmRef clientId = do
  csm <- atomically $ readTVar csmRef
  case csm ^. at clientId of
    Just CONNECTED -> return True
    _ -> return False

handleInboxRequest :: ClientInbox
                   -> (BayeuxInternalMsg, Cloud.SendPort [BayeuxInternalMsg])
                   -> Cloud.Process ()
handleInboxRequest clientInbox (ClientInboxRequest, sPort) = do
    msgs <- liftIO . atomically $ do
              msg <- readTChan clientInbox
              readInboxContents [msg]
    Cloud.sendChan sPort msgs
  where
    readInboxContents acc = do
      shouldStop <- isEmptyTChan clientInbox
      if shouldStop
         then return (reverse acc)
         else do
           msg <- readTChan clientInbox
           readInboxContents (msg:acc)
handleInboxRequest _ _ = error "Sending invalid sync message to client"

--------------------------------------------------------------------------------

handleBayeuxMsg :: ClientInbox -> BayeuxInternalMsg -> Cloud.Process ()
handleBayeuxMsg clientInbox response = do
    liftIO $ atomically (writeTChan clientInbox response)

--------------------------------------------------------------------------------

spawnClient :: ClientId -> Cloud.Process ()
spawnClient clientId = do
    clientInbox <- liftIO $ newClientInbox

    clientInputPid <- Cloud.spawnLocal $ forever $
         Cloud.receiveWait [
           Cloud.match (handleBayeuxMsg clientInbox)
         , Cloud.match (mapM_ (handleBayeuxMsg clientInbox))
         ]

    clientOutputPid <- Cloud.spawnLocal $ forever $
         Cloud.receiveWait [Cloud.match (handleInboxRequest clientInbox)]

    Cloud.register ("client.input." ++ clientId) clientInputPid
    Cloud.register ("client.output." ++ clientId) clientOutputPid
