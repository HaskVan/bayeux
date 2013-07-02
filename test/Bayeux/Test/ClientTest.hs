{-# LANGUAGE OverloadedStrings #-}
module Bayeux.Test.ClientTest (specs) where

import           Control.Lens                  ((^.))

--------------------

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.STM        (atomically, readTVar)

--------------------

import           Test.Hspec                    (Spec, context, describe, it,
                                                pending)
import           Test.HUnit                    (assertBool, assertEqual)

--------------------

import           Bayeux.Internal.Client
import           Bayeux.Internal.Engine.Memory (spawnEngine')
import           Bayeux.Internal.Types

import           Bayeux.Util.Test              (execTestContext)

--------------------------------------------------------------------------------

specs = describe "Bayeux.Client" $ do

   context "newClientState" $ do
     it "creates a new client with CONNECTING status" $ do
       client <- execTestContext $ spawnEngine' >> newClientState'
       status <- atomically $ readTVar (client ^. clientStateStatus)
       assertEqual "client status should be CONNECTING" CONNECTING status

     it "creates a new client with a ClientId" $ do
       client <- execTestContext $ spawnEngine' >> newClientState'
       assertBool "ClientId should not be empty" $ (length $ client ^. clientStateId) > 0

   context "connectClient" $ do
     it "updates the clientStateStatus to CONNECTED" $ do
       client <- execTestContext $ do
                   spawnEngine'
                   client <- newClientState'
                   connectClient' client
                   return client
       threadDelay 500
       cs <- atomically (readTVar (client ^. clientStateStatus))
       assertEqual "ClientStatus should be CONNECTED" CONNECTED cs

   context "publish/subscribe" $ do
     it "publish updates client inbox when subscribed" $ do
      (client1, client2) <- execTestContext $ do
                  spawnEngine'
                  client1 <- newClientState'
                  client2 <- newClientState'
                  subscribe' client1 "/hello"
                  subscribe' client2 "/hello"
                  publish' client1 "/hello" "Hello World"
                  return (client1, client2)
      let cid1  = client1 ^. clientStateId
      let cid2  = client2 ^. clientStateId
      inbox1 <- readInboxContents client1
      inbox2 <- readInboxContents client2
      let result1 = [
             Response (ConnectRequest   cid1)
           , Response (SubscribeRequest cid1 "/hello")
           , Response (PublishRequest   cid1 "/hello" "Hello World")
           , PublishRequest cid1 "/hello" "Hello World"]
      let result2 = [
             Response (ConnectRequest   cid2)
           , Response (SubscribeRequest cid2 "/hello")
           , PublishRequest cid1 "/hello" "Hello World"]
      assertEqual "should have a PublishRequest on inbox" result1 inbox1
      assertEqual "should have a PublishRequest on inbox" result2 inbox2
