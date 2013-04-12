module Bayeux.Test.ContextTest (specs) where

--------------------

import           Control.Concurrent      (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Trans     (MonadIO (..))

--------------------

import           Test.Hspec              (Spec, context, describe, it)
import           Test.HUnit              (assertEqual)

--------------------

import           Bayeux.Internal.Context (mkContext, withContext)
import           Bayeux.Internal.Engine  (sendToEngineSync)
import           Bayeux.Internal.Types   (BayeuxInternalMsg (Ping, Pong),
                                          Context)

--------------------------------------------------------------------------------

specs :: Spec
specs = return ()

  --       describe "Bayeux.Context" $ do
  -- describe "mkContext" $ do
  --   it "starts the bayeux engine" $ do
  --     ctx <- mkContext
  --     withContext ctx $ do
  --       spawnEngine
  --       result <- sendToEngineSync Ping
  --       liftIO $ putMVar promise result
  --     result <- takeMVar promise
  --     assertEqual "bayeux engine is not up" Pong result
