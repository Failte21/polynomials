module BigOSpec where

import           Data.Map                      as Map

import           SpecHelper

spec :: Spec
spec = do
  describe "createLoopMap"
    $          context "with []"
    $          it "Should be []"
    $          createLoopMap baseMap
    `shouldBe` expected
 where
  baseMap  = Map.fromList []
  expected = Map.fromList []
