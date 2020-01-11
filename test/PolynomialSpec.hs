module PolynomialSpec where

import           SpecHelper

spec :: Spec
spec = do
  describe "multP"
    $          context "with [1, 1] [1, 1]"
    $          it "Should be [1, 2, 1]"
    $          multP [1, 1] [1, 1]
    `shouldBe` [1, 2, 1]

  describe "multP"
    $          context "with [0, 0, 3] [2, 3]"
    $          it "Should be [0, 0,, 6, 9]"
    $          multP [0, 0, 3] [2, 3]
    `shouldBe` [0, 0, 6, 9]
