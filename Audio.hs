module Audio where

import qualified Data.Vector.Storable as F
import qualified Data.Map as M
import qualified Sound.File.Sndfile as S
import Sound.File.Sndfile.Buffer.Vector (fromBuffer, toBuffer)
import Control.Exception (try)


type Audio = M.Map Int (F.Vector Double)

{- This is written because either I don't understand something
   or the default implementation of slice over storable 
   vectors is buggy since it does not chop the start off  -}

slice :: Int -> Int -> F.Vector Double -> F.Vector Double
slice start end =
  F.slice 0 (end - start) . F.drop start

zeroedSlice :: Int -> Int -> F.Vector Double -> F.Vector Double
zeroedSlice start end vector =
  left F.++ slice start' end' vector F.++ right
  where start' = max start 0
        end' = min end $ F.length vector
        left = F.replicate (negate $ min 0 start) 0
        right = F.replicate (max 0 $ end - F.length vector) 0

sliceAudio :: Int -> Int -> Audio -> Audio 
sliceAudio start end =
  M.map (zeroedSlice start end)


mixAudio :: [Audio] -> Audio
mixAudio = M.unionsWith $ F.zipWith (+)

audioLength :: Audio -> Int
audioLength = F.length . head . M.elems

separate :: Int -> F.Vector Double -> Audio 
separate n vector =
  M.fromList
    [(p, F.ifilter noMod $ F.drop p vector)
     | p <- [1..n]]
  where noMod p _ = p `mod` n == 0

type Routing = M.Map Int Int


route :: Routing -> Audio -> Audio
route routing audio =
  mixAudio $ map getAndRename $ M.keys routing

  where getAndRename port = case do
          channel <- M.lookup port audio
          targetPort <- M.lookup port routing
          return $ M.singleton targetPort channel
          of Just out -> out
             Nothing -> M.empty



readAudio :: String -> IO (Maybe Audio)
readAudio path = do
  readResult <- try $ S.readFile path
  return $ case readResult of
    Left (S.SystemError e) -> Nothing
    Right (info, samples) ->
      separate (S.channels info) . fromBuffer <$> samples



