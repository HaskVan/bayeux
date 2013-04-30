module Bayeux.Test.Engine.MemoryTest where

--------------------

import           Control.Concurrent            (newEmptyMVar, putMVar, takeMVar,
                                                threadDelay)
import           Control.Monad.Trans           (MonadIO (..))

--------------------

import qualified Control.Distributed.Process   as Cloud

--------------------

import           Test.Hspec                    (Spec, context, describe, it,
                                                pending)
import           Test.HUnit                    (assertBool, assertEqual)

--------------------

import           Bayeux.Internal.Context       (runContextOnce)
import           Bayeux.Internal.Engine.Memory
import           Bayeux.Internal.Messages      (sendHandshake)
import           Bayeux.Internal.Types         (BayeuxInternalMsg (..), Context)

--------------------------------------------------------------------------------

execContext :: Cloud.Process a -> IO a
execContext action = do
  result <- newEmptyMVar
  runContextOnce (action >>= liftIO . putMVar result)
  takeMVar result

specs :: Spec
specs = describe "Bayeux.Engine.Memory" $ do

  context "handshake" $ do
    it "returns a ClientId on response" $ do
      result <- execContext $ spawnEngine' >> sendHandshake
      assertBool "clientId length must be greater than zero" $ length result > 0

    it "adds an entry to the ClientStatusMap" $ pending
    it "adds a CONNECTING entry to ClientStatusMap" $ pending

  context "connect" $ do
    context "unknown clientId" $ do
      it "returns invalid response" $ pending
    context "known clientId" $ do
      it "returns valid connect response" $ pending
      it "updates entry on ClientStatusMap to CONNECTED" $ pending
