module Bayeux.Internal.Context where

--------------------

import           Control.Applicative              (pure, (<$>), (<*>))
import           Control.Exception                (SomeException (..), try)

--------------------

import qualified Control.Distributed.Process      as Cloud
import qualified Control.Distributed.Process.Node as Cloud
import qualified Network.Transport                as Transport
import qualified Network.Transport.Chan           as Transport

--------------------

import           Bayeux.Internal.Engine           (spawnEngine)
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

releaseContext :: Context -> IO ()
releaseContext ctx = do
    result <- try $ Cloud.closeLocalNode (_contextNodeId ctx)
    case result of
      Left (SomeException _) -> return ()
      _ -> return ()
    return ()

withContext :: Context -> Cloud.Process () -> IO ()
withContext ctx process = Cloud.runProcess (_contextNodeId ctx) process

runContextOnce :: Cloud.Process () -> IO ()
runContextOnce process = do
  ctx <- mkContext
  withContext ctx process
  releaseContext ctx
