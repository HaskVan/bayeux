module Bayeux.Internal.Context where

--------------------------------------------------------------------------------
import Data.Applicative ((<$>), (<*>))
import qualified Control.Distributed.Process      as Cloud
import qualified Control.Distributed.Process.Node as Cloud
import qualified Network.Transport                as Transport
import qualified Network.Transport.Chan           as Transport

--------------------------------------------------------------------------------
import           Bayeux.Internal.Types            (Context (..))

--------------------------------------------------------------------------------

initLocalNode :: IO (Transport.Transport, Cloud.LocalNode)
initLocalNode = do
    transport <- Transport.createTransport
    (,) <$> (pure transport)
        <*> Cloud.newLocalNode transport Cloud.initRemoteTable

mkContext :: IO Context
mkContext = do
    (transport, localNode) <- initLocalNode
    Context <$> (pure localNode)
            <*> (pure transport)
