module Bayeux.Test.Engine.MemoryTest where

--------------------

import           Control.Concurrent          (newEmptyMVar, putMVar, takeMVar,
                                              threadDelay)
import           Control.Monad.Trans         (MonadIO (..))

--------------------

import qualified Control.Distributed.Process as Cloud

--------------------

import           Test.Hspec                  (Spec, context, describe, it,
                                              pending)
import           Test.HUnit                  (assertBool, assertEqual)

--------------------

import           Bayeux.Internal.Context     (runContextOnce)
import           Bayeux.Internal.Types       (BayeuxInternalMsg (..), Context)

--------------------------------------------------------------------------------

specs :: Spec
specs = describe "Bayeux.Engine.Memory" $ do

  context "handshake" $ do
    it "returns a ClientId on response" $ pending
    it "adds an entry to the ClientStatusMap" $ pending
    it "adds a CONNECTING entry to ClientStatusMap" $ pending

  context "connect" $ do
    context "unknown clientId" $ do
      it "returns invalid response" $ pending
    context "known clientId" $ do
      it "returns valid connect response" $ pending
      it "updates entry on ClientStatusMap to CONNECTED" $ pending
