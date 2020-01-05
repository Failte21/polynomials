module Phoityne.Example.MathSpec where

import           SpecHelper

spec :: Spec
spec =
  describe "sumGs"
    $          context "with [1..10]"
    $          it "Should be 55"
    $          sumG 1 10
    `shouldBe` 55

main :: IO ()
main = hspec spec
