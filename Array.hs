module Array where

import qualified Data.Vector as V
import qualified Data.Map as M

type AxesSize   = V.Vector Int
type MultiIndex = V.Vector Int


data Array a = Array AxesSize (V.Vector a)


flatIdx :: AxesSize -> MultiIndex -> Int
flatIdx s is = 
  V.sum $ V.zipWith (*) (V.scanr (*) 1 (V.tail s)) is


aget :: Array a -> MultiIndex -> a
aget (Array is x) i = x V.! (flatIdx is i)


aset :: Array a -> [(MultiIndex, a)] -> Array a
aset (Array is x) iv =
  Array is $ x V.// map (\(a, b) -> (flatIdx is a, b)) iv


aimap :: (Int -> a -> b) -> Array a -> Array b
aimap f (Array is x) = Array is $ V.imap f x





