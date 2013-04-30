module Bayeux.Util.IdGenerator where

import           Test.QuickCheck     (resize, sample')
import           Test.QuickCheck.Gen (Gen (..), elements, listOf1)

idChars :: String
idChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genId :: IO String
genId = fmap head
             (sample' .
              resize 30 .
              listOf1 $
              elements idChars)
