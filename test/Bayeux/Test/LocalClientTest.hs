module Bayeux.Test.LocalClientTest (tests) where

import           Bayeux.Internal.LocalClient (newLocalClient)
import           Bayeux.Internal.Types       (LocalClient)
import           Test.Hspec                  (Spec, context, describe, it,
                                              shouldBe)


tests :: Spec
tests = do
  describe "LocalClient" $ do
    it "must be dummy" $ do
      r <- newLocalClient
      case r of
        Right localClient -> True `shouldBe` True
        Left e -> False `shouldBe` False
