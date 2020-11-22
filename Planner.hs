{-# LANGUAGE RecordWildCards #-}

module Planner where

import Data.Bifunctor

import qualified Control.Exception as E
import qualified Audio as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as Q

import Array (aget, aset, Array, MultiIndex, aimap, flatIdx)

compareBy f a b = f a `compare` f b

data Tempo = Tempo
  {tempoStart :: Double,
   bpm        :: Double}

{- consider caching tempo maps on each slider tick, if it performs
   lousy. O(1) access time is not guaranteed by the compiler-}

defaultTempi = [Tempo 0 120] 

deleteTempo :: Double -> [Tempo] -> [Tempo]
deleteTempo slider = filter $ (slider /=) . tempoStart

setTempo :: Double -> Double -> [Tempo] -> [Tempo]
setTempo slider bpm =
    L.insertBy (compareBy tempoStart) (Tempo slider bpm)
  . deleteTempo slider



{- be extra careful, I screwed those functions when renaming
   variables several times and didn't notice -}

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
softTempoChange n slider (t1 : ts)

  | null ts || slider < tempoStart t1 =
      E.throw $ E.IndexOutOfBounds "you should catch this"

  | slider >= tempoStart t2 = t1 : softTempoChange n slider ts

  | tempoStart t1 == slider = take n $ zipWith Tempo
    [slider, slider + l ..] [bpm t1, bpm t1 + d ..] ++ ts
  
  where t2 = head ts
        l  = (tempoStart t2 - slider) / fromIntegral n
        d  = (bpm t2 - bpm t1) / fromIntegral n


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
   chunkFX     :: [Effect],
   link        :: Either MultiIndex A.OnDisk,
   routing     :: A.Routing}


data Track = Bus
  {busName   :: String,
   controls  :: [(A.Routing, Track)],
   busFX     :: [Effect]}
  | Ragged
  {raggedName   :: String,
   schedules    :: [[Chunk]],
   raggedFX     :: [Effect],
   pool         :: Array A.OnDisk}


{- class declaration here is actually superfluous -}
mix :: [(A.Audio, m)] -> (A.Audio, [m])
mix = first A.mixAudio . unzip


fromTrack :: Double
          -> (Chunk -> Bool)
          -> (Chunk -> Double)
          -> (Double -> (Double, Double))
          -> Track
          -> (A.Audio, Track)

fromTrack quartSamp inScope speed borders t@(Ragged {..}) =

  let continue c@(Chunk {..})
        | inScope c = (out, c {chunkFX=newFX})
        | otherwise = (A.empty, c)
        where onDisk = either (aget pool) id link
              source = case onDisk of
                (A.Unloaded _) -> error "you shouldn't load on playback"
                (A.Loaded _ source) -> source
              realPos      = round . (* speed c)
              (at, len)    = bimap realPos realPos $ borders chunkStart
              sliced       = A.sliceAudio slaveStart slaveEnd at len source
              routed       = A.route routing sliced
              (out, newFX) = applyFX quartSamp routed chunkFX

      {- looks ugly but does the job :) -}
      (raw, newSchedules) = mix $ map (mix . map continue) schedules

      (out, newFX) = applyFX quartSamp raw raggedFX

  in (out, t {schedules=newSchedules, raggedFX=newFX})



fromTrack quartSamp inScope speed borders t@(Bus _ controls fx) =

  let sameWith = fromTrack quartSamp inScope speed borders
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

        speed (Chunk {..}) = onMap / onSource
          where onMap    = window rate chunkStart chunkEnd tempi
                onSource = fromIntegral $ slaveEnd - slaveStart

        borders start    = (window rate slider start tempi, buffLen)
        (out, newMaster) = fromTrack quartSamp inScope speed borders master







