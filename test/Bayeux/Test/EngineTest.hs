module Bayeux.Test.EngineTest (specs) where

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

import           Bayeux.Internal.Client      (getMessagesFromClientInbox,
                                              spawnClient)
import           Bayeux.Internal.Context     (runContextOnce)
import           Bayeux.Internal.Engine      (sendHandshake, sendToEngine,
                                              sendToEngineSync, spawnEngine)
import           Bayeux.Internal.Types       (BayeuxInternalMsg (..), Context)


--------------------------------------------------------------------------------

specs :: Spec
specs = describe "Bayeux.Engine" $ do

  describe "spawnEngine" $ do
    it "starts the engine thread" $ do
      promise <- newEmptyMVar
      runContextOnce $ do
        spawnEngine
        result <- sendToEngineSync Ping
        liftIO $ putMVar promise result
      result <- takeMVar promise
      assertEqual "bayeux engine is not up" Pong result

  describe "sendHandshake" $ do
    it "returns a new clientId" $ do
      promise <- newEmptyMVar
      runContextOnce $ do
        spawnEngine
        result <- sendHandshake
        liftIO $ putMVar promise result
      result <- takeMVar promise
      assertBool "should return clientId" $ (length result) > 0

  describe "sendBayeuxMessage" $ do

    it "ConnectRequest message" $ do
      promise <- newEmptyMVar
      runContextOnce $ do
        spawnEngine
        clientId  <- sendHandshake
        spawnClient clientId
        sendToEngine [ConnectRequest clientId]
        result <- getMessagesFromClientInbox clientId
        liftIO $ putMVar promise result
      result <- takeMVar promise
      assertEqual "should receive a valid response" [ConnectResponse] result

    context "client not connected" $ do

      it "SubscribeRequest must fail" $ do
        promise <- newEmptyMVar
        runContextOnce $ do
          spawnEngine
          --
          clientId  <- sendHandshake
          let bayeuxMsg = SubscribeRequest clientId "/hello"
          spawnClient clientId
          --
          sendToEngine [bayeuxMsg]
          result <- getMessagesFromClientInbox clientId
          liftIO $ putMVar promise result
        result <- takeMVar promise
        case result of
          [ErrorResponse {}] -> assertBool "" True
          _ ->  assertBool "should receive an error response" False

    context "client connected" $ do

      it "SubscribeRequest must be success" $ pending

    -- it "Send message with fake clientId" pending
    -- it "Send subscribe message" pending
    -- it "Send publish message" pending
    -- it "Send unpublish message" pending
    -- it "Send disconnect message" pending
