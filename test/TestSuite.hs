module Main where

import           Test.Hspec        (Spec, describe, it, shouldBe)
import           Test.Hspec.Runner (Config (..), defaultConfig, hspecWith)
--------------------------------------------------------------------------------
-- import qualified Bayeux.Test.ContextTest     as Context
-- import qualified Bayeux.Test.LocalClientTest as LocalClient

dummySpec :: Spec
dummySpec = describe "This is an example" $ do
    it "always returns True" $ True `shouldBe` True
    it "returns False" $ True `shouldBe` True

tests :: Spec
tests = do
  dummySpec
  -- LocalClient.tests

runTests :: IO ()
runTests = do
    putStrLn (replicate 20 '-')
    hspecWith userConfig tests >> return ()
    putStrLn ""
  where
    userConfig = defaultConfig {
                   configFastFail = True
                 }

main :: IO ()
main = runTests
