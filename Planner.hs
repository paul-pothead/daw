module Planner where

import qualified Audio as A
import qualified Data.List as L
import qualified Data.Map as M

import Array (aget, aset, Array)

compareBy f a b = f a `compare` f b

type Knobs = M.Map String Double


data Tempo = Tempo
  {tempoStart :: Double,
   bpm        :: Double}

deleteTempo :: Double -> [Tempo] -> [Tempo]
deleteTempo slider = filter $ (slider /=) . tempoStart

addTempo :: Double -> Double -> [Tempo] -> [Tempo]
addTempo start bpm =
  L.insertBy (compareBy tempoStart) (Tempo start bpm)



bufferBeats :: Int -> Double -> Double -> [Tempo] -> Double
bufferBeats rate slider sampLeft (t1 : ts)

  | slider < tempoStart t1 = bufferBeats rate slider sampLeft ts

  | null ts || sampTillNext >= sampLeft = sampLeft / rate' / 60 / bpm t1
  
  | otherwise = tempoStart t2 - slider
              + bufferBeats rate (tempoStart t2) (sampLeft - sampTillNext) ts

  where rate' = fromIntegral rate
        t2 = head ts
        sampTillNext = 60 * rate' * (tempoStart t2 - slider) / bpm t1



softTempoChange :: Int -> Double -> [Tempo] -> [Tempo]
softTempoChange n slider (t1 : tail@(t2 : ts))

  | slider < tempoStart t1 = t1 : softTempoChange n slider tail

  | tempoStart t1 == slider = take n $ zipWith Tempo
    [slider, slider + l ..] [bpm t1, bpm t1 + d ..] ++ tail
  
  where l = (tempoStart t2 - slider) / fromIntegral n
        d = (bpm t2 - bpm t1) / fromIntegral n



windowSamples :: Int -> Double -> Double -> [Tempo] -> Int
windowSamples rate start end (t1 : ts)
  
  | start > end = negate (windowSamples rate end start ts)

  | start < tempoStart t1 = windowSamples rate start end ts

  | null ts || end <= tempoStart t2 = sampTillNext

  | otherwise = sampTillNext + windowSamples rate (tempoStart t2) end ts

  where t2 = head ts
        sampTillNext = round $ 60 * fromIntegral rate * (end - start) / bpm t1



noteSamples :: Int -> Double -> Int -> [Tempo] -> Int
noteSamples rate start divisor tempi =
  windowSamples rate start (4 / fromIntegral divisor + start) tempi



data Effect = Effect
  {effectName :: String,
   knobs      :: Knobs,
   run        :: (Int -> A.Audio -> Knobs -> (A.Audio, Effect))}

applyFX :: Int -> A.Audio -> [Effect] -> (A.Audio, [Effect])
applyFX quarterSamples =
  L.mapAccumL (\audio effect ->
    run effect quarterSamples audio $ knobs effect)




data Chunk = Chunk
  {chunkStart  :: Double,
   chunkEnd    :: Double,
   slaveStart  :: Int,
   link        :: [Int],
   routing     :: A.Routing}


pickChunks :: Double -> Double -> [Chunk] -> [Chunk]
pickChunks from to schedule =
  [x | x <- schedule, chunkStart x < to, chunkEnd x >= from]

data Track = Bus
  {busName :: String,
   controls :: [(A.Routing, Track)],
   busFX   :: [Effect]}
  | Ragged
  {raggedName :: String,
   schedule   :: [Chunk],
   raggedFX   :: [Effect],
   pool       :: Array Track}
  | Source
  {sourceName :: String,
   speed    :: Double,
   sourceFX   :: [Effect],
   audio    :: A.Audio}

isRagged (Ragged _ _ _ _) = True
isRagged _ = False

mix :: [(A.Audio, Track)] -> (A.Audio, [Track])
mix audioList =
  (A.mixAudio $ map fst audioList, map snd audioList)


fromSource :: Int -> Int -> Int -> Track -> (A.Audio, Track)
fromSource quarterSamp at until (Source name speed fx audio) =
  (out, Source name speed newFX audio)

  where at' = round $ fromIntegral at * speed
        until' = round $ fromIntegral until * speed
        toSend = A.sliceAudio at' until' audio
        (out, newFX) = applyFX quarterSamp toSend fx


fromRagged :: [Tempo] -> Int -> Double -> Int -> Track -> (A.Audio, Track)
fromRagged tempi rate slider buffSamp (Ragged name schedule fx pool) =
  (out, Ragged name schedule newFX newPool)

  where quarterSamp = noteSamples rate slider 4 tempi
        end = slider + bufferBeats rate slider (fromIntegral buffSamp) tempi
        inScope = pickChunks slider end schedule

        continue chunk = (A.route (routing chunk) out, newTrack)
          where slave = aget pool $ link chunk
                offset = windowSamples rate (chunkStart chunk) slider tempi
                at = slaveStart chunk + offset
                until = at + buffSamp
                (out, newTrack) = fromSource quarterSamp at until slave
        
        (raw, newTracks) = mix $ map continue inScope
        (out, newFX) = applyFX quarterSamp raw fx
        newPool = aset pool $ zip (map link inScope) newTracks


fromBus :: [Tempo] -> Int -> Double -> Int -> Track -> (A.Audio, Track)
fromBus tempi rate slider buffSamp (Bus name controls fx) =
  (out, Bus name nowControls newFX)

  where quarterSamp = noteSamples rate slider 4 tempi

        continue (routing, track) = (A.route routing out, newTrack)
          where (out, newTrack) =
                 (if isRagged track
                  then fromRagged
                  else fromBus) tempi rate slider buffSamp track

        (raw, newTracks) = mix $ map continue controls
        nowControls = zip (map fst controls) newTracks
        (out, newFX) = applyFX quarterSamp raw fx


data Project = Project
  {name    :: String,
   tempi   :: [Tempo],
   master  :: Track,
   slider  :: Double,
   rate    :: Int,
   buffLen :: Int}


