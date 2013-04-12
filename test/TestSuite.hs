{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module Main where

import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner       (Config (..), defaultConfig, hspecWith)
--------------------------------------------------------------------------------

import qualified Bayeux.Test.ClientTest
import qualified Bayeux.Test.ContextTest
import qualified Bayeux.Test.EngineTest


specs :: Spec
specs = do
  Bayeux.Test.ContextTest.specs
  Bayeux.Test.EngineTest.specs
  Bayeux.Test.ClientTest.specs

runTests :: IO ()
runTests = do
    hspecWith userConfig specs >> return ()
  where
    userConfig = defaultConfig {
                   configFastFail = True
                 }

main :: IO ()
main = runTests
