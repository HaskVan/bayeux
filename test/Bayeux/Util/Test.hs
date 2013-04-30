module Bayeux.Util.Test where

import           Control.Monad.Trans         (MonadIO (..))

import           Control.Concurrent          (newEmptyMVar, putMVar, takeMVar)

import qualified Control.Distributed.Process as Cloud

import           Bayeux.Internal.Context     (runContextOnce)

execTestContext :: Cloud.Process a -> IO a
execTestContext action = do
  result <- newEmptyMVar
  runContextOnce (action >>= liftIO . putMVar result)
  takeMVar result
