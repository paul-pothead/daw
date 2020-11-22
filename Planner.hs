{-# LANGUAGE RecordWildCards #-}

module Planner where

import Data.Bifunctor

import qualified Audio as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as Q

import Array (aget, aset, Array, MultiIndex)

compareBy f a b = f a `compare` f b

data Tempo = Tempo
  {tempoStart :: Double,
   bpm        :: Double}

defaultTempi = [Tempo 0 120] 

deleteTempo :: Double -> [Tempo] -> [Tempo]
deleteTempo slider = filter $ (slider /=) . tempoStart

addTempo :: Double -> Double -> [Tempo] -> [Tempo]
addTempo slider bpm =
    L.insertBy (compareBy tempoStart) (Tempo slider bpm)
  . deleteTempo slider



tick :: Double -> Double -> Double -> [Tempo] -> Double
tick rate slider buffSamp (t1 : ts)

  | null ts                  = newSlider
  | slider >= tempoStart t2  = tick rate slider buffSamp ts
  | sampTillNext >= buffSamp = newSlider
  
  | otherwise = tick rate (tempoStart t2) (buffSamp - sampTillNext) ts

  where t2           = head ts
        sampTillNext = 60 * rate * (tempoStart t2 - slider) / bpm t1
        newSlider    = buffSamp / rate / 60 * bpm t1 + tempoStart t1


softTempoChange :: Int -> Double -> [Tempo] -> [Tempo]
softTempoChange n slider (t1 : tail@(t2 : ts))

  | slider < tempoStart t1 = t1 : softTempoChange n slider tail

  | tempoStart t1 == slider = take n $ zipWith Tempo
    [slider, slider + l ..] [bpm t1, bpm t1 + d ..] ++ tail
  
  where l = (tempoStart t2 - slider) / fromIntegral n
        d = (bpm t2 - bpm t1) / fromIntegral n



window :: Double -> Double -> Double -> [Tempo] -> Double
window rate start end (t1 : ts)
  
  | start > end = negate (window rate end start ts)
  | null ts     = samples $ end - tempoStart t1

  | start >= tempoStart t2 = window rate start end ts
  | end <= tempoStart t2   = samples $ end - tempoStart t1

  | otherwise = samples (tempoStart t2 - tempoStart t1)
              + window rate (tempoStart t2) end ts

  where t2            = head ts
        samples beats = 60 * rate * beats / bpm t1



noteSamples :: Double -> Double -> Double -> [Tempo] -> Double
noteSamples rate start divisor tempi =
  window rate start (4 / divisor + start) tempi

data State = State
  {knobs :: M.Map String Double,
   accum :: Q.Seq A.Audio}


data Effect = Effect
  {effectName :: String,
   state      :: State,
   run        :: (Double -> A.Audio -> State -> (A.Audio, Effect))}

applyFX :: Double -> A.Audio -> [Effect] -> (A.Audio, [Effect])
applyFX quartSamp =
  L.mapAccumL (\audio effect ->
    run effect quartSamp audio $ state effect)


data Chunk = Chunk
  {chunkStart  :: Double,
   chunkEnd    :: Double,
   slaveStart  :: Int,
   slaveEnd    :: Int,
   speed       :: Double,
   chunkFX     :: [Effect],
   link        :: Either MultiIndex A.OnDisk,
   routing     :: A.Routing}




data Track = Bus
  {busName   :: String,
   controls  :: [(A.Routing, Track)],
   busFX     :: [Effect]}
  | Ragged
  {raggedName   :: String,
   schedule     :: [Chunk],
   raggedFX     :: [Effect],
   pool         :: Array A.OnDisk}


class Mixable m where
  mix :: [(A.Audio, m)] -> (A.Audio, [m])
  mix = first A.mixAudio . unzip

instance Mixable Track
instance Mixable Chunk


fromTrack :: Double
          -> (Chunk -> Bool)
          -> (Double -> (Double, Double))
          -> Track
          -> (A.Audio, Track)

fromTrack quartSamp inScope borders t@(Ragged _ schedule fx pool) =

  let continue c@(Chunk {..})
        | inScope c = (out, c {chunkFX=newFX})
        | otherwise = (A.empty, c)
        where onDisk = either (aget pool) id link
              source = case onDisk of
                (A.Unloaded _) -> error "you shouldn't load on playback"
                (A.Loaded _ source) -> source
              realPos      = round . (speed *)
              (at, len)    = bimap realPos realPos $ borders chunkStart
              sliced       = A.sliceAudio slaveStart slaveEnd at len source
              routed       = A.route routing sliced
              (out, newFX) = applyFX quartSamp routed chunkFX
      
      (raw, newSchedule) = mix $ continue <$> schedule
      (out, newFX) = applyFX quartSamp raw fx

  in (out, t {schedule=newSchedule, raggedFX=newFX})



fromTrack quartSamp inScope borders t@(Bus _ controls fx) =

  let sameWith = fromTrack quartSamp inScope borders
      continue (routing, track) =
        A.route routing `first` sameWith track

      (raw, newTracks) = mix $ continue <$> controls
      nowControls = zip (fst <$> controls) newTracks
      (out, newFX) = applyFX quartSamp raw $ fx

  in (out, t {controls=nowControls, busFX=newFX})


data Project = Project
  {name    :: String,
   tempi   :: [Tempo],
   master  :: Track,
   slider  :: Double,
   rate    :: Double,
   buffLen :: Double}


fill :: Project -> (A.Audio, Project)
fill p@(Project {..}) =
  (out, p {slider=newSlider, master=newMaster})

  where quartSamp = noteSamples rate slider 4 tempi
        newSlider = tick rate slider buffLen tempi
        inScope x = chunkStart x < newSlider && chunkEnd x >= slider
        borders start = (window rate slider start tempi, buffLen)
        (out, newMaster) = fromTrack quartSamp inScope borders master







