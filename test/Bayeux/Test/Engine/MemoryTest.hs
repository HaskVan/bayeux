module Bayeux.Test.Engine.MemoryTest where

import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet

--------------------

import           Control.Lens                  (at, (^.))

--------------------

import           Control.Concurrent            (newEmptyMVar, putMVar, takeMVar,
                                                threadDelay)
import           Control.Concurrent.STM        (atomically, readTVar)
import           Control.Monad.Trans           (MonadIO (..))

--------------------

import qualified Control.Distributed.Process   as Cloud

--------------------

import           Test.Hspec                    (Spec, context, describe, it,
                                                pending)
import           Test.HUnit                    (assertBool, assertEqual)

--------------------

import           Bayeux.Internal.Engine.Memory
import           Bayeux.Internal.Messages      (sendHandshake, sendToEngine)
import           Bayeux.Internal.Types

import           Bayeux.Util.Test              (execTestContext)

--------------------------------------------------------------------------------


specs :: Spec
specs = describe "Bayeux.Engine.Memory" $ do

  context "handshake" $ do

    it "returns a ClientId on response" $ do
      result <- execTestContext $ spawnEngine' >> sendHandshake
      assertBool "clientId length must be greater than zero" $ length result > 0

    it "adds an entry to the ClientStatusMap" $ do
       engine <- newEngineState
       statusMap0 <- atomically $ readTVar (engine ^. engineStateClientStatusMap)
       assertBool "engine state map should be empty" $ (HashMap.size statusMap0) == 0
       _ <- execTestContext $ spawnEngine'' engine >> sendHandshake
       statusMap <- atomically $ readTVar (engine ^. engineStateClientStatusMap)
       assertBool "engine state map should be have one clientId" $ (HashMap.size statusMap) == 1

    it "adds a CONNECTING entry to ClientStatusMap" $ do
       engine <- newEngineState
       cid <- execTestContext $ spawnEngine'' engine >> sendHandshake
       statusMap <- atomically $ readTVar (engine ^. engineStateClientStatusMap)
       assertEqual "engine state map should have clientId with CONNECTING"
                   (Just CONNECTING) (statusMap ^. at cid)

  context "connect" $ do

    context "unknown clientId" $ do

      it "doesn't update the client status to CONNECTED" $ do
        engine <- newEngineState
        _ <- execTestContext $ spawnEngine'' engine >> sendToEngine [ConnectRequest "foo"]
        statusMap <- atomically $ readTVar (engine ^. engineStateClientStatusMap)
        assertBool "engine state map should be empty" $ (HashMap.size statusMap) == 0

    context "known clientId" $ do

      it "updates entry on ClientStatusMap to CONNECTED" $ do
        engine <- newEngineState
        cid <- execTestContext $ do
                 spawnEngine'' engine
                 cid <- sendHandshake
                 sendToEngine [ConnectRequest cid]
                 return cid
        threadDelay 100
        statusMap <- atomically $ readTVar (engine ^. engineStateClientStatusMap)
        assertBool  "engine state map should not be empty" (HashMap.size statusMap > 0)
        assertEqual "engine state map should have clientId with CONNECTED"
                     (Just CONNECTED) (statusMap ^. at cid)

  context "subscribe" $ do

     context "when client is not CONNECTED" $ do
       it "doesn't modify engine subscription map" $ do
         engine <- newEngineState
         cid <- execTestContext $ do
                  spawnEngine'' engine
                  cid <- sendHandshake
                  sendToEngine [SubscribeRequest cid "/hello"]
                  return cid
         threadDelay 100
         subscriptionMap <- atomically $ readTVar (engine ^. engineStateSubscriptions)
         assertBool "engine subscription map should be empty" (HashMap.size subscriptionMap == 0)

     context "when client is CONNECTED" $ do
       it "adds cid to chan name on subscription map" $ do
         engine <- newEngineState
         cid <- execTestContext $ do
                  spawnEngine'' engine
                  cid <- sendHandshake
                  sendToEngine [ConnectRequest cid, SubscribeRequest cid "/hello"]
                  return cid
         threadDelay 100
         subscriptionMap <- atomically $ readTVar (engine ^. engineStateSubscriptions)
         assertBool "engine subscription map should have 1 channel" (HashMap.size subscriptionMap == 1)
         assertEqual "engine subscription map should have a channel with one clientId"
                     (Just $ HashSet.fromList [cid]) (subscriptionMap  ^. at "/hello")

  -- context "publish" $ do

  --   it "sends a message to each subscriber on the channel" $ do
  --     execTestContext $ do
  --       spawnEngine'
  --       cid1 <- sendHandshake
  --       cid2 <- sendHandshake
  --       sendToEngine [ConnectRequest cid1, SubscribeRequest cid1 "/hello"]
  --       sendToEngine [ConnectRequest cid2, SubscribeRequest cid2 "/hello"]
  --       sendToEngine [PublishRequest cid1 "/hello" (
