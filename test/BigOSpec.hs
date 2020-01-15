module BigOSpec where

import           Data.Map                      as Map

import           SpecHelper

spec :: Spec
spec = do
  describe "createLoopMap"
    $          context "with []"
    $          it "Should be []"
    $          createLoopMap baseMapEmpty
    `shouldBe` expectedEmpty

  describe "createLoopMap"
    $          context "with baseMap"
    $          it "Should be expected"
    $          createLoopMap baseMap
    `shouldBe` expected

  describe "reduce"
    $          context "with baseMap loopMap"
    $          it "Should be []"
    $          reduce baseMap expected
    `shouldBe` [1]

 where
  baseMapEmpty  = Map.fromList []
  expectedEmpty = Map.fromList []
  expected      = Map.fromList [aLoops, bLoops, cLoops, dLoops]
  baseMap =
    Map.fromList [("a", aNode), ("b", bNode), ("c", cNode), ("d", dNode)]
  aNode  = ([1], ["b"])
  bNode  = ([], ["c"])
  cNode  = ([], ["d", "a"])
  dNode  = ([], ["c"])
  aLoops = ("a", [["a", "b", "c"]])
  bLoops = ("b", [])
  cLoops = ("c", [["c", "d"]])
  dLoops = ("d", [])

