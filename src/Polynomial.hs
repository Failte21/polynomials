module Polynomial
    ( mult
    , multP
    )
where

import           Data.List                     as List

type Polynomial = [Int]

mult :: Polynomial -> Int -> Polynomial
mult poly value = List.map (* value) poly

multPAux :: Polynomial -> Polynomial -> Polynomial
multPAux []        p2        = p2
multPAux p1        []        = p1
multPAux (h1 : t1) (h2 : t2) = (h1 * h2) : multPAux t1 t2

multP :: Polynomial -> Polynomial -> Polynomial
multP []        p2        = p2
multP p1        []        = p1
multP (h1 : t1) (h2 : t2) = (h1 + h2) : multPAux t1 t2

eq :: Int -> Polynomial -> Int -> Polynomial
eq value poly n = poly ++ List.map (const value) [0 .. n]

eq1 = eq 1
eq0 = eq 0
