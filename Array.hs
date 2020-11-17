module Array where

import Data.Vector (Vector, (!), (//))

type AxesSize = [Int]
type MultiIndex = [Int]

data Array a = Array AxesSize (Vector a)

flatIdx :: AxesSize -> MultiIndex -> Int
flatIdx s i = 
  sum $ zipWith (*) (scanr (*) 1 (tail s)) i

aget :: Array a -> MultiIndex -> a
aget (Array is x) i =
  x ! (flatIdx is i)

aset :: Array a -> [(MultiIndex, a)] -> Array a
aset (Array is x) iv =
  Array is $ x // map (\(a, b) -> (flatIdx is a, b)) iv




