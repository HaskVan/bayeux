{-# LANGUAGE TemplateHaskell #-}
module Bayeux.Internal.Engine.Memory where

import           Data.HashMap.Strict                      (HashMap)

import qualified Data.HashMap.Strict                      as HashMap

--------------------

import           Control.Concurrent.STM                   (TVar, atomically,
                                                           modifyTVar,
                                                           newTVarIO, readTVar)

--------------------

import           Control.Applicative                      ((<$>), (<*>))
import           Control.Lens                             (at, (&), (.~), (^.))
import           Control.Lens.TH                          (makeLenses)
import           Control.Monad                            (forever)
import           Control.Monad.Trans                      (MonadIO (..))

--------------------

import qualified Control.Distributed.Process              as Cloud
import qualified Control.Distributed.Process.Serializable as Cloud

--------------------

import           Bayeux.Util.IdGenerator                  (genId)

import           Bayeux.Internal.Context                  (withContext)
import           Bayeux.Internal.Messages                 (sendToClient)
import           Bayeux.Internal.Types                    (BayeuxInternalMsg (..),
                                                           ChanName, ClientId,
                                                           ClientStatus (..),
                                                           Context)

-- Types -----------------------------------------------------------------------

data EngineState
    = EngineState {
      _engineStateSubscriptions   :: TVar (HashMap ChanName ClientId)
    , _engineStateClientStatusMap :: TVar (HashMap ClientId ClientStatus)
    }

makeLenses ''EngineState

newEngineState :: IO EngineState
newEngineState = EngineState <$> newTVarIO HashMap.empty
                             <*> newTVarIO HashMap.empty

isClientConnected :: EngineState -> ClientId -> IO Bool
isClientConnected engine cid = do
  csm <- atomically $ readTVar (engine ^. engineStateClientStatusMap)
  case csm ^. at cid of
    Just CONNECTED -> return True
    _ -> return False

updateClientStatus :: EngineState -> ClientId -> ClientStatus -> IO ()
updateClientStatus engine cid clientSt =
    atomically $ modifyTVar (engine ^. engineStateClientStatusMap)
                            updateClientStatus'
  where
    updateClientStatus' csm = csm & at cid .~ (Just clientSt)


-- Cloud Handlers --------------------------------------------------------------

handleSyncMsg :: EngineState
              -> (BayeuxInternalMsg, Cloud.SendPort BayeuxInternalMsg)
              -> Cloud.Process ()
handleSyncMsg _ (Ping, sPort) = Cloud.sendChan sPort Pong
handleSyncMsg _ (HandshakeRequest, sPort) = do
    liftIO $ putStrLn "[engine] Handling Handshake request"
    clientId <- liftIO genId
    Cloud.sendChan sPort $ HandshakeResponse clientId
handleSyncMsg _ msg = error $ show msg ++ ": Message not supported"

handleMsg :: EngineState -> BayeuxInternalMsg -> Cloud.Process ()
handleMsg engine (ConnectRequest clientId) = do
    liftIO $ updateClientStatus engine clientId CONNECTED
    sendToClient clientId ConnectResponse
handleMsg engine msg@(SubscribeRequest clientId chanName) = do
    isClientConnected' <- liftIO $ isClientConnected engine clientId
    if not isClientConnected'
       then sendToClient clientId (ErrorResponse msg)
       else return ()
handleMsg _ msg = error $ show msg ++ ": Message not supported yet"

-- Initialization of Engine ----------------------------------------------------

spawnEngine :: Context -> IO ()
spawnEngine ctx = withContext ctx $ spawnEngine'

spawnEngine' :: Cloud.Process ()
spawnEngine' = do
  engine <- liftIO $ newEngineState
  spawnEngine'' engine

spawnEngine'' :: EngineState -> Cloud.Process ()
spawnEngine'' engine = do
    enginePid <- Cloud.spawnLocal $ handleMessages
    Cloud.register "bayeux.engine" enginePid
  where
    handleMessages = forever $ do
      Cloud.receiveWait [ Cloud.match (handleSyncMsg engine)
                        , Cloud.match (mapM_ (handleMsg engine)) ]
