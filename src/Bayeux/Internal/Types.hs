module Bayeux.Internal.Types where

--------------------------------------------------------------------------------
import           Control.Concurrent          (MVar)
import           Data.ByteString             (ByteString)
import           Data.HashMap.Strict         (HashMap)

--------------------------------------------------------------------------------
import qualified Control.Distributed.Process as Cloud
import qualified Network.Transport           as Transport


--------------------------------------------------------------------------------

type ChanName = String
type ClientId = String
type BayeuxCallback = ByteString -> IO ()

data Context
  = Context {
    _contextNodeId :: Cloud.LocalNode
  , _contextTransport :: Transport.Transport
  }
  deriving (Show)

data LocalClient
  = LocalClient {
    _localClientId        :: ClientId
  , _localClientProcessId :: Cloud.ProcessId
  , _localClientCallbacks :: MVar (HashMap ChanName [BayeuxCallback])
  }
