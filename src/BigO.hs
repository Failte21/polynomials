module BigO
  ( createLoopMap
  )
where

import           Polynomial                     ( Polynomial
                                                , multP
                                                )
import           Data.Map                      as Map
import           Data.List                     as List
import           Debug.Trace

type Cost = Polynomial
type Relation = String
type Node = (Cost, [Relation])

type BaseMap = Map String Node
type LoopMap = Map String [[Relation]]
type KeyMap = Map [Relation] Bool

getLoopKeys
  :: String
  -> (String, Node)
  -> Map Relation Bool
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
                                            updatedExplored
                                            baseMap
                                            sortedPath
                                            acc
  )
  acc
  relations
 where
  updatedExplored = Map.insert nodeName True explored
  relations       = snd node
  sortedPath      = sort updatedPath
  updatedPath     = nodeName : path

createLoopMap :: BaseMap -> LoopMap
createLoopMap baseMap = loopMap where
  (loopMap, _) = List.foldl
    (\(loopMap, loopIdsMap) (key, node) ->
      let explored = Map.empty
      in  let loopKeys = getLoopKeys key (key, node) explored baseMap [] []
          in  let filteredKeys = List.filter
                    (\loopKey -> not $ Map.member loopKey loopIdsMap)
                    loopKeys
              in  let updatedLoopsMap = Map.insert key filteredKeys loopMap
                  in  let updatedLoopIdsMap = List.foldl
                            (\acc k -> Map.insert k True acc)
                            loopIdsMap
                            filteredKeys
                      in  (updatedLoopsMap, updatedLoopIdsMap)
    )
    (loopMap, loopIdsMap)
    nodeList   where
    acc        = (loopMap, loopIdsMap)
    loopMap    = Map.empty
    loopIdsMap = Map.empty
    nodeList   = Map.toList baseMap


reduce :: BaseMap -> LoopMap -> Polynomial
reduce baseMap loopMap = []
