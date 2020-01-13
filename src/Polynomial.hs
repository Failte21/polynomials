module Polynomial
    ( multP
    , Polynomial
    )
where

import           Data.List                     as List

type Polynomial = [Int]

addPAux :: Polynomial -> Polynomial -> Polynomial -> Polynomial
addPAux poly1     []        acc = acc ++ poly1
addPAux []        poly2     acc = acc ++ poly2
addPAux (h1 : t1) (h2 : t2) acc = addPAux t1 t2 acc ++ [h1 + h2]

addP :: Polynomial -> Polynomial -> Polynomial
addP poly1 poly2 = addPAux poly1 poly2 []

enumerateAux :: [Int] -> [(Int, Int)] -> Int -> [(Int, Int)]
enumerateAux []      acc _ = acc
enumerateAux (h : t) acc i = enumerateAux t (acc ++ [(h, i)]) (i + 1)

enumerate :: [Int] -> [(Int, Int)]
enumerate array = enumerateAux array [] 0

pad :: Int -> [Int]
pad i | i <= 0 = []
pad i          = 0 : pad (i - 1)

multSingleAux :: [Int] -> Int -> [Int]
multSingleAux []      _   = []
multSingleAux (h : t) val = (h * val) : multSingleAux t val

multSingle :: (Int, Int) -> [Int] -> [Int]
multSingle (value, n) array = padded ++ multSingleAux array value
    where padded = pad n

multP :: Polynomial -> Polynomial -> Polynomial
multP poly1 poly2 = reverse standard  where
    standard =
        foldl (\acc enu -> addP acc (multSingle enu poly2)) [] enumeratedPoly1
    enumeratedPoly1 = enumerate poly1
