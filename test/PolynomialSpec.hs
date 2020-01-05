module PolynomialSpec where

import           SpecHelper

spec :: Spec
spec = do
  describe "mult"
    $          context "with [1, 1, 1] 1"
    $          it "Should be [1, 1, 1]"
    $          mult [1, 1, 1] 1
    `shouldBe` [1, 1, 1]
  describe "multP"
    $          context "with [1, 1, 1] [1, 2]"
    $          it "Should be [2, 2, 1]"
    $          multP [1, 1, 1] [1, 2]
    `shouldBe` [2, 2, 1]
