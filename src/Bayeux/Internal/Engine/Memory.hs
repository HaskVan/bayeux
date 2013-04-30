{-# LANGUAGE TemplateHaskell #-}
module Bayeux.Internal.Engine.Memory where

import           Data.HashMap.Strict                      (HashMap)
import           Data.HashSet                             (HashSet)
import           Data.Traversable                         (Traversable (..),
                                                           traverse)

import qualified Data.HashMap.Strict                      as HashMap
import qualified Data.HashSet                             as HashSet

--------------------

import           Control.Concurrent.STM                   (TVar, atomically,
                                                           modifyTVar,
                                                           newTVarIO, readTVar)

--------------------

import           Control.Applicative                      (Applicative, (<$>),
                                                           (<*>))
import           Control.Lens                             (at, mapMOf_, to,
                                                           (%~), (&), (.~),
                                                           (^.))
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

-- Types & Basic state management ----------------------------------------------

data EngineState
    = EngineState {
      _engineStateSubscriptions   :: TVar (HashMap ChanName (HashSet ClientId))
    , _engineStateClientStatusMap :: TVar (HashMap ClientId ClientStatus)
    }

makeLenses ''EngineState

--------------------

newEngineState :: (Applicative m, MonadIO m) => m EngineState
newEngineState = EngineState <$> (liftIO $ newTVarIO HashMap.empty)
                             <*> (liftIO $ newTVarIO HashMap.empty)

isClientConnected :: MonadIO m => EngineState -> ClientId -> m Bool
isClientConnected engine cid = do
  csm <- liftIO . atomically $ readTVar (engine ^. engineStateClientStatusMap)
  case csm ^. at cid of
    Just CONNECTED -> return True
    _ -> return False

updateClientStatus :: MonadIO m => EngineState -> ClientId -> ClientStatus -> m ()
updateClientStatus engine cid clientSt =
    liftIO . atomically $ modifyTVar (engine ^. engineStateClientStatusMap)
                            updateClientStatus'
  where
    updateClientStatus' csm = csm & at cid .~ (Just clientSt)

addClientIdToChannel :: MonadIO m => EngineState -> ChanName -> ClientId -> m ()
addClientIdToChannel engine chanName cid =
    liftIO . atomically $ modifyTVar (engine ^. engineStateSubscriptions)
                                     updateSubscription'
  where
    updateSubscription' sm = sm & at chanName %~ addCidToChan
    addCidToChan Nothing = Just $ HashSet.singleton cid
    addCidToChan (Just subs) = Just $ HashSet.insert cid subs

sendToClientsInChannel :: EngineState -> ChanName -> BayeuxInternalMsg -> Cloud.Process ()
sendToClientsInChannel engine chanName msg = do
    subscriptions <- liftIO . atomically $ readTVar (engine ^. engineStateSubscriptions)
    subscriptions ^. at chanName
                  & mapMOf_ (traverse . (to HashSet.toList) . traverse)
                            (\cid -> sendToClient cid msg)

-- Cloud Handlers --------------------------------------------------------------

handleSyncMsg :: EngineState
              -> (BayeuxInternalMsg, Cloud.SendPort BayeuxInternalMsg)
              -> Cloud.Process ()
handleSyncMsg engine (HandshakeRequest, sPort) = do
    liftIO $ putStrLn "[engine] Handling Handshake request"
    cid <- liftIO genId
    liftIO $ updateClientStatus engine cid CONNECTING
    Cloud.sendChan sPort $ HandshakeResponse cid
handleSyncMsg _ msg = error $ show msg ++ ": Message not supported"

handleMsg :: EngineState -> BayeuxInternalMsg -> Cloud.Process ()
handleMsg engine msg@(ConnectRequest cid) = do
    liftIO $ putStrLn "Receiving connect request"
    updateClientStatus engine cid CONNECTED
    sendToClient cid (Response msg)
handleMsg engine msg@(SubscribeRequest cid chanName) = do
    isClientConnected' <- isClientConnected engine cid
    if not isClientConnected'
       then sendToClient cid (ErrorResponse msg)
       else do
         addClientIdToChannel engine chanName cid
         sendToClient cid (Response msg)
handleMsg engine msg@(PublishRequest cid chanName payload) = do
    isClientConnected' <- isClientConnected engine cid
    if not isClientConnected'
       then sendToClient cid (ErrorResponse msg)
       else do
         sendToClient cid (Response msg)
         sendToClientsInChannel engine chanName msg

handleMsg _ msg = error $ show msg ++ ": Message not supported yet"

-- Initialization of Engine ----------------------------------------------------

spawnEngine :: Context -> IO ()
spawnEngine ctx = withContext ctx $ spawnEngine'

spawnEngine' :: Cloud.Process ()
spawnEngine' = do
  engine <- newEngineState
  spawnEngine'' engine

spawnEngine'' :: EngineState -> Cloud.Process ()
spawnEngine'' engine = do
    enginePid <- Cloud.spawnLocal $ handleMessages
    Cloud.register "bayeux.engine" enginePid
  where
    handleMessages = forever $ do
      Cloud.receiveWait [ Cloud.match (handleSyncMsg engine)
                        , Cloud.match (handleMsg engine)
                        , Cloud.match (mapM_ (handleMsg engine)) ]
