module Audio where

import qualified Data.Vector.Storable as F
import qualified Data.Map as M
import qualified Sound.File.Sndfile as S
import Sound.File.Sndfile.Buffer.Vector (fromBuffer, toBuffer)
import Control.Exception (try)


{- Internal formats other than 4 byte floats per sample
   are not supported, this is a design choice. -}

type Audio = M.Map Int (F.Vector Float)
empty = M.empty


{- I'm afraid, a more or less detailed guitar soundbank will
   eat up 50GB minimum so we unload and load records into RAM
   when editing a score, keeping those absolutely necessary for playback.
   Another interesting option would be asynchronous import on playback,
   but this will quickly turn into hell :) -}

data OnDisk = Unloaded String | Loaded String Audio



zeroedSlice :: Int -> Int -> Int -> Int -> F.Vector Float -> F.Vector Float
zeroedSlice masterStart masterEnd start len vector =
  left F.++ F.slice start' len' vector F.++ right
  where realStart = masterStart + start
        start' = max 0 $ min masterEnd realStart
        len' = min len $ masterEnd - start'
        leftLen = negate $ min start 0
        left = F.replicate leftLen 0
        right = F.replicate (len - leftLen - len') 0


sliceAudio :: Int -> Int -> Int -> Int -> Audio -> Audio 
sliceAudio masterStart masterEnd start len =
  M.map (zeroedSlice masterStart masterEnd start len)


mixAudio :: [Audio] -> Audio
mixAudio = M.unionsWith $ F.zipWith (+)

audioLength :: Audio -> Int
audioLength = F.length . head . M.elems


{- TODO check efficiency and possible alternatives -}
separate :: Int -> F.Vector Float -> Audio 
separate n vector = M.fromList
  [(p, F.ifilter (\i _ -> i `mod` n == 0) $ F.drop p vector)
   | p <- [0..n-1]]


{- source and target ports respectively -}
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


