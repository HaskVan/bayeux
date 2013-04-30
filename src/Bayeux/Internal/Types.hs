{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE TemplateHaskell    #-}
module Bayeux.Internal.Types where

--------------------

import           Data.HashMap.Strict              (HashMap)
import           Data.Set                         (Set)

--------------------

import           Control.Concurrent.STM           (TVar)
import           Control.Concurrent.STM.TChan     (TChan)
import           Control.Lens                     (makeLenses)
-- import           Data.ByteString                  (ByteString)
-- import           Data.HashMap.Strict              (HashMap)

--------------------

import           Data.Binary                      (Binary (..), getWord8,
                                                   putWord8)
import           Data.Derive.Binary               (makeBinary)
import           Data.DeriveTH                    (derive)
import           Data.Typeable                    (Typeable)

--------------------


import qualified Control.Distributed.Process      as Cloud
import qualified Control.Distributed.Process.Node as Cloud
import qualified Network.Transport                as Transport


--------------------------------------------------------------------------------

type ChanName = String

data ClientStatus
    = CONNECTING
    | CONNECTED
    | DISCONNECTED
    deriving (Show, Eq, Ord, Typeable)

type ClientPid = Cloud.ProcessId
type ClientId = String

--------------------------------------------------------------------------------

data Context
  = Context {
    _contextNodeId    :: Cloud.LocalNode
  , _contextTransport :: Transport.Transport
  }

makeLenses ''Context

data BayeuxInternalMsg
    = Ping
    | Pong
    | Response BayeuxInternalMsg
    | ErrorResponse BayeuxInternalMsg
    | ClientInboxRequest
    | HandshakeRequest
    | HandshakeResponse ClientId
    | ConnectRequest ClientId
    | SubscribeRequest ClientId ChanName
    | PublishRequest ClientId ChanName String
    deriving (Show, Eq, Ord, Typeable)

$(derive makeBinary ''BayeuxInternalMsg)

type ClientInbox = TChan BayeuxInternalMsg

--------------------------------------------------------------------------------

type ClientStatusMap = TVar (HashMap ClientId ClientStatus)
type ClientSubscriptionMap = TVar (HashMap ChanName (Set ClientId))

data EngineState
    = EngineState {
      _engineClientStatusMap     :: ClientStatusMap
    , _engineClientSubscriptions :: ClientSubscriptionMap
    }

makeLenses ''EngineState

--------------------------------------------------------------------------------

data ClientState
    = ClientState {
      _clientStateId     :: ClientId
    , _clientStateStatus :: TVar ClientStatus
    , _clientStateInbox  :: ClientInbox
    }

makeLenses ''ClientState

-- type ChanName = String
-- type ClientId = String
-- type BayeuxCallback = ByteString -> IO ()

-- data LocalClient
--   = LocalClient {
--     _localClientId        :: ClientId
--   , _localClientProcessId :: Cloud.ProcessId
--   , _localClientCallbacks :: MVar (HashMap ChanName [BayeuxCallback])
--   }
