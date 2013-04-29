{-# LANGUAGE TemplateHaskell #-}
module Bayeux.Internal.Engine.Memory where

import           Data.HashMap.Strict    (HashMap)

import qualified Data.HashMap.Strict    as HashMap

--------------------

import           Control.Concurrent.STM (TVar, newTVarIO)

--------------------

import Control.Applicative ((<$>), (<*>))
import           Control.Lens.TH        (makeLenses)

--------------------

import           Bayeux.Internal.Types  (ChanName, ClientId, ClientStatus)

--------------------------------------------------------------------------------

data EngineState
    = EngineState {
      _engineStateSubscriptions   :: TVar (HashMap ChanName ClientId)
    , _engineStateClientStatusMap :: TVar (HashMap ClientId ClientStatus)
    }

makeLenses ''EngineState

newEngineState = EngineState <$> newTVarIO HashMap.empty
                             <*> newTVarIO HashMap.empty

--------------------
