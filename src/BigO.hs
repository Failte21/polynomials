module BigO
  ()
where

import           Polynomial                     ( Polynomial
                                                , multP
                                                )
import           Data.Map                      as Map
import           Data.List                     as List

type Cost = Polynomial
type Relation = String
type Node = (Cost, [Relation])

type BaseMap = Map String Node
type LoopMap = Map String [[Relation]]
type KeyMap = Map [Relation] Bool

getLoopKeys
  :: String
  -> (String, Node)
  -> BaseMap
  -> BaseMap
  -> [Relation]
  -> [[Relation]]
  -> [[Relation]]
getLoopKeys baseNode (nodeName, _) explored _ path acc
  | member nodeName explored && nodeName == baseNode = path : acc
getLoopKeys baseNode (nodeName, _) explored _ _ acc
  | member nodeName explored = acc
getLoopKeys baseNode (nodeName, node) explored baseMap path acc = List.foldl
  (\acc relation ->
    let nodeToExploreMaybe = Map.lookup relation baseMap
    in  case nodeToExploreMaybe of
          Nothing            -> acc
          Just nodeToExplore -> getLoopKeys baseNode
                                            (relation, nodeToExplore)
                                            explored
                                            baseMap
                                            sortedPath
                                            acc
  )
  acc
  relations
 where
  relations   = snd node
  sortedPath  = sort updatedPath
  updatedPath = nodeName : path

createLoopMap :: BaseMap -> LoopMap
-- createLoopMap baseMap = List.foldl
--   (\(knownLoops, loopMap) (key, node) ->
--     let loopKeys = getLoopKeys (key, node) explored baseMap [] []
--     in  let filteredKeys = List.filter
--               (\loopKey -> Map.member loopKey knownLoops == False)
--               loopKeys
--         in  let updatedLoopsMap = Map.insert key filteredKeys loopMap
--             in  let updatedKnownLoops = List.foldl
--                       (\acc k -> Map.insert k True acc)
--                       knownLoops
--                       filteredKeys
--                 in  (updatedKnownLoops, updatedLoopsMap)
--   )

--   (explored, emptyMap)
--   baseTuples where
--   baseTuples = toList baseMap
--   emptyMap   = fromList []
--   keyMap     = fromList []
--   explored   = fromList []
createLoopMap _ = fromList []
