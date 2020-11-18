module Planner where

import Data.Bifunctor

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
   pool       :: Array Source}

data Source = Source
  {sourceName :: String,
   speed    :: Double,
   sourceFX   :: [Effect],
   audio    :: A.Audio}


mix = first A.mixAudio . unzip

fromSource :: Int -> (Int, Int) -> Source -> (A.Audio, Source)
fromSource quarterSamp (at, until) (Source name speed fx audio) =
  (out, Source name speed newFX audio)

  where at' = round $ fromIntegral at * speed
        until' = round $ fromIntegral until * speed
        toSend = A.sliceAudio at' until' audio
        (out, newFX) = applyFX quarterSamp toSend fx


fromTrack :: Int
           -> ([Chunk] -> [Chunk])
           -> (Chunk -> (Int, Int))
           -> Track
           -> (A.Audio, Track)
fromTrack quartSamp inScope borders (Ragged name schedule fx pool) =
  let continue chunk = A.route (routing chunk) `first`
        fromSource quartSamp (borders chunk) (pool `aget` link chunk)
      visible = inScope schedule
      (raw, newSources) = mix $ continue <$> visible
      (out, newFX) = applyFX quartSamp raw fx
      newPool = pool `aset` zip (link <$> visible) newSources
  in (out, Ragged name schedule newFX newPool)

fromTrack quartSamp inScope borders (Bus name controls fx) =
  let continue (routing, track) = A.route routing `first`
        fromTrack quartSamp inScope borders track
      (raw, newTracks) = mix $ continue <$> controls
      nowControls = zip (fst <$> controls) newTracks
      (out, newFX) = applyFX quartSamp raw fx 
  in (out, Bus name nowControls newFX)
 

data Project = Project
  {name    :: String,
   tempi   :: [Tempo],
   master  :: Track,
   slider  :: Double,
   rate    :: Int,
   buffLen :: Int}

fill :: Project -> (A.Audio, Project)
fill p@(Project name tempi master slider rate buffLen) =
  (out, p {slider=newSlider, master=newMaster})
  where quartSamp = noteSamples rate slider 4 tempi
        newSlider = slider
                  + bufferBeats rate slider (fromIntegral buffLen) tempi
        inScope = pickChunks slider newSlider
        borders chunk =
          let at = windowSamples rate slider (chunkStart chunk) tempi
              until = at + buffLen
          in (at, until)
        (out, newMaster) = fromTrack quartSamp inScope borders master







