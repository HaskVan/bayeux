module Bayeux.Test.ContextTest (tests) where


import           Bayeux.Internal.Context (mkContext)
import           Bayeux.Internal.Types   (Context)
import           Test.Hspec              (Spec, context, describe, it)
import qualified Test.HUnit              as Test

assertRight :: (Show a, Show b) => Either a b -> Test.Assertion
assertRight val@(Left _) =
    Test.assertBool ((show val) ++ " should have been Right, was Left")
                    False
assertRight _ = Test.assertBool "" True

tests :: Spec
tests = do
  describe "Context" $ do
    it ".mkContext" $ do
      context <- mkContext
      assertRight context
