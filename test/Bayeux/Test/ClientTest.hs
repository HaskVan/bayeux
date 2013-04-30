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
       threadDelay 100
       cs <- atomically (readTVar (client ^. clientStateStatus))
       assertEqual "ClientStatus should be CONNECTED" CONNECTED cs
