{-# LANGUAGE RecordWildCards #-}

module Planner where

import Data.Bifunctor

import qualified Audio as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as Q

import Array (aget, aset, Array)

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
  | end <= tempoStart t2 = samples $ end - tempoStart t1

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
   link        :: [Int],
   routing     :: A.Routing}


pickChunks :: Double -> Double -> [Chunk] -> [Chunk]
pickChunks from to schedule =
  [x | x <- schedule, chunkStart x < to, chunkEnd x >= from]


data Track = Bus
  {busName   :: String,
   controls  :: [(A.Routing, Track)],
   busFX     :: [Effect],
   busFrozen :: Maybe Source}
  | Ragged
  {raggedName   :: String,
   schedule     :: [Chunk],
   raggedFX     :: [Effect],
   pool         :: Array Source,
   raggedFrozen :: Maybe Source}

data Source = Source
  {sourceName   :: String,
   speed        :: Double,
   sourceFX     :: [Effect],
   audio        :: A.Audio}


class Mixable m where
  mix :: [(A.Audio, m)] -> (A.Audio, [m])
  mix = first A.mixAudio . unzip

instance Mixable Source
instance Mixable Track


class Tickable t where
  

fromSource :: Double -> (Double, Double) -> Source -> (A.Audio, Source)

fromSource quartSamp (at, until) s@(Source _ speed fx audio) =
  (out, s {sourceFX=newFX})

  where at' = round $ at * speed
        until' = round $ until * speed
        toSend = A.sliceAudio at' until' audio
        (out, newFX) = applyFX quartSamp toSend fx



fromTrack :: Double
          -> ([Chunk] -> [Chunk])
          -> (Double -> (Double, Double))
          -> Track
          -> (A.Audio, Track)

fromTrack quartSamp _ borders t@(Ragged _ _ _ _ (Just f)) =
  let (out, newFrozen) = fromSource quartSamp (borders 0) f
  in (out, t {busFrozen=Just newFrozen})


fromTrack quartSamp inScope borders t@(Ragged _ schedule fx pool _) =
  let continue (Chunk start _ link routing) = A.route routing `first`
        fromSource quartSamp (borders start) (aget pool link)

      visible = inScope $ schedule
      (raw, newSources) = mix $ continue <$> visible
      (out, newFX) = applyFX quartSamp raw $ fx
      newPool = pool `aset` zip (link <$> visible) newSources

  in (out, t {raggedFX=newFX, pool=newPool})


fromTrack quartSamp _ borders t@(Bus _ _ _ (Just f)) =
  let (out, newFrozen) = fromSource quartSamp (borders 0) f
  in (out, t {busFrozen=Just newFrozen})


fromTrack quartSamp inScope borders t@(Bus _ controls fx _) =
  let continue (routing, track) = A.route routing `first`
        fromTrack quartSamp inScope borders track

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
        inScope = pickChunks slider newSlider
        borders start =
          let at = window rate slider start tempi
              until = at + buffLen
          in (at, until)
        (out, newMaster) = fromTrack quartSamp inScope borders master







