{- |
'Clock's are the central new notion in Rhine.
There are clock types (instances of the 'Clock' type class)
and their values.
This module provides the 'Clock' type class, several utilities,
and certain general constructions of 'Clock's,
such as clocks lifted along monad morphisms or time rescalings.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
  -- ( module FRP.Rhine.Clock
  -- )

module FRP.Rhine.Clock where

import Control.Monad (class Monad, bind, pure, (<$>))
import Data.MonadicStreamFunction (MSF, arrM, morphS)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Effect (Effect)
import FRP.Rhine.TimeDomain (class TimeDomain, DateTime)

import Control.Category as Category
import Control.Monad.Trans.Class (lift, class MonadTrans)
import Data.Profunctor (arr)
import Data.Profunctor.Strong ((***), first)
import Data.Time.Duration (Milliseconds)
import Effect.Class (liftEffect, class MonadEffect)
import Prelude (($), (>>>))

import Data.NaturalTransformation (type (~>))

-- * The 'Clock' type class

{- |
A clock creates a stream of time stamps and additional information,
possibly together with side effects in a monad 'm'
that cause the environment to wait until the specified time is reached.
-}
type RunningClock m time tag = MSF m Unit (Tuple time tag)

{- |
When initialising a clock, the initial time is measured
(typically by means of a side effect),
and a running clock is returned.
-}
type RunningClockInit m time tag = m (Tuple (RunningClock m time tag) time)

{- |
Since we want to leverage Haskell's type system to annotate signal networks by their clocks,
each clock must be an own type, 'cl'.
Different values of the same clock type should tick at the same speed,
and only differ in implementation details.
Often, clocks are singletons.
-}
-- class TimeDomain (Time cl) => Clock m cl where
--   -- | The time domain, i.e. type of the time stamps the clock creates.
--   type Time cl
--   -- | Additional information that the clock may output at each tick,
--   --   e.g. if a realtime promise was met, if an event occurred,
--   --   if one of its subclocks (if any) ticked.
--   type Tag cl
--   -- | The method that produces to a clock value a running clock,
--   --   i.e. an effectful stream of tagged time stamps together with an initialisation time.
--   initClock
--     :: cl -- ^ The clock value, containing e.g. settings or device parameters
--     -> RunningClockInit m (Time cl) (Tag cl) -- ^ The stream of time stamps, and the initial time

class TimeDomain time diff <= Clock m cl time tag diff | cl -> time tag diff where
  -- time
  -- | The time domain, i.e. type of the time stamps the clock creates.
  -- tag
  -- | Additional information that the clock may output at each tick,
  --   e.g. if a realtime promise was met, if an event occurred,
  --   if one of its subclocks (if any) ticked.
  -- | The method that produces to a clock value a running clock,
  --   i.e. an effectful stream of tagged time stamps together with an initialisation time.
  initClock
    :: cl -- ^ The clock value, containing e.g. settings or device parameters
    -> RunningClockInit m time tag -- ^ The stream of time stamps, and the initial time

-- * Auxiliary definitions and utilities

-- | An annotated, rich time stamp.
data TimeInfo cl time tag diff = TimeInfo
  { -- | Time passed since the last tick
    sinceLast :: diff
    -- | Time passed since the initialisation of the clock
  , sinceInit :: diff
    -- | The absolute time of the current tick
  , absolute  :: time
    -- | The tag annotation of the current tick
  , tag       :: tag
  }

-- | A utility that changes the tag of a 'TimeInfo'.
-- Removed (time1 ~ time2) constraint... 
retag
  :: forall time tag1 tag2 cl1 cl2 diff
   . (tag1 -> tag2)
  -> TimeInfo cl1 time tag1 diff -> TimeInfo cl2 time tag2 diff
retag f (TimeInfo ti@{ tag }) = TimeInfo $ ti { tag = f tag }

-- * Certain universal building blocks to produce new clocks from given ones

-- ** Rescalings of time domains

-- | A pure morphism of time domains is just a function.
type Rescaling cl timefrom timeto = timefrom -> timeto

-- | An effectful morphism of time domains is a Kleisli arrow.
--   It can use a side effect to rescale a point in one time domain
--   into another one.
type RescalingM m cl timefrom timeto = timefrom -> m timeto

-- | An effectful, stateful morphism of time domains is an 'MSF'
--   that uses side effects to rescale a point in one time domain
--   into another one.
type RescalingS m cl timefrom tagfrom timeto tagto = MSF m ((timefrom) /\ (tagfrom)) (timeto /\ tagto)

-- | Like 'RescalingS', but allows for an initialisation
--   of the rescaling morphism, together with the initial time.
type RescalingSInit m cl timefrom tagfrom timeto tagto = timefrom -> m ((RescalingS m cl timefrom tagfrom timeto tagto) /\ timeto)

-- | Convert an effectful morphism of time domains into a stateful one with initialisation.
--   Think of its type as @RescalingM m cl time -> RescalingSInit m cl time tag@,
--   although this type is ambiguous.
rescaleMToSInit
  :: forall m time1 time2 tag
   . Monad m
  => (time1 -> m time2) -> time1 -> m ((MSF m (time1 /\ tag) (time2 /\ tag)) /\ time2)
rescaleMToSInit rescaling time1 = go <$> rescaling time1
  where
    go t = (arrM rescaling *** Category.identity) /\ t


-- ** Applying rescalings to clocks

-- | Applying a morphism of time domains yields a new clock.
data RescaledClock cl time timeto = RescaledClock
  { unscaledClock :: cl
  , rescale       :: Rescaling cl time timeto
  }


-- instance (Monad m, TimeDomain time, Clock m cl)
--       => Clock m (RescaledClock cl time) where
--   type Time (RescaledClock cl time) = time
--   type Tag  (RescaledClock cl time) = Tag cl
--   initClock (RescaledClock cl f) = do
--     (runningClock, initTime) <- initClock cl
--     return
--       ( runningClock >>> first (arr f)
--       , f initTime
--       )

instance rescaledClockutcTimeDomain
  :: (Monad m, TimeDomain DateTime Milliseconds, Clock m cl DateTime tag Milliseconds)
  => Clock m (RescaledClock cl DateTime DateTime) DateTime tag Milliseconds
  where
  -- type Time (RescaledClock cl time) = time
  -- type Tag  (RescaledClock cl time) = Tag cl
  -- -> m (Tuple (RunningClock m time tag) time)
  initClock (RescaledClock {unscaledClock, rescale}) = do
    (runningClock /\ initTime) <- initClock unscaledClock
    pure
      -- runningClock :: MSF m Unit (Tuple time tag)
      -- rescale :: time -> timeto
      ( runningClock >>> (first (arr rescale))
      /\ rescale initTime
      )

-- -- | Instead of a mere function as morphism of time domains,
-- --   we can transform one time domain into the other with an effectful morphism.
-- data RescaledClockM m cl time timeto = RescaledClockM
--   { unscaledClockM :: cl
--   -- ^ The clock before the rescaling
--   , rescaleM       :: RescalingM m cl time timeto
--   -- ^ Computing the new time effectfully from the old time
--   }

-- instance rescaledClockM :: (Monad m, TimeDomain time diff, Clock m cl)
--       => Clock m (RescaledClockM m cl time) time tag diff where
--   initClock (RescaledClockM { unscaledClockM, rescaleM }) = do
--     (runningClock /\ initTime) <- initClock unscaledClockM
--     rescaledInitTime         <- rescaleM initTime
--     pure
--       ( runningClock >>> fst (arrM rescaleM)
--       /\ rescaledInitTime
--       )

-- -- | A 'RescaledClock' is trivially a 'RescaledClockM'.
-- rescaledClockToM 
--   :: forall m cl time timeto
--    . Monad m 
--   => RescaledClock cl time timeto -> RescaledClockM m cl time timeto
-- rescaledClockToM (RescaledClock { unscaledClock, rescale }) = RescaledClockM
--   { unscaledClockM : unscaledClock
--   , rescaleM       : pure <<< rescale
--   }


-- -- | Instead of a mere function as morphism of time domains,
-- --   we can transform one time domain into the other with a monadic stream function.
-- data RescaledClockS m cl time tag timeto tagto = RescaledClockS
--   { unscaledClockS :: cl
--   -- ^ The clock before the rescaling
--   , rescaleS       :: RescalingSInit m cl time tag timeto tagto
--   -- ^ The rescaling stream function, and rescaled initial time,
--   --   depending on the initial time before rescaling
--   }

-- instance rescaledClockS :: (Monad m, TimeDomain time, Clock m cl)
--       => Clock m (RescaledClockS m cl time tag) time tag diff where
--   initClock (RescaledClockS { unscaledClockS, rescaleS }) = do
--     (runningClock /\ initTime) <- initClock unscaledClockS
--     (rescaling /\ rescaledInitTime) <- rescaleS initTime
--     pure
--       ( runningClock >>> rescaling
--       /\ rescaledInitTime
--       )

-- -- | A 'RescaledClockM' is trivially a 'RescaledClockS'.
-- rescaledClockMToS
--   :: forall m cl time tag timeto tag. Monad m
--   => RescaledClockM m cl time timeto -> RescaledClockS m cl time tag timeto tag
-- rescaledClockMToS (RescaledClockM { unscaledClockM, rescaleM }) = RescaledClockS
--   { unscaledClockS : unscaledClockM
--   , rescaleS       : rescaleMToSInit rescaleM
--   }

-- -- | A 'RescaledClock' is trivially a 'RescaledClockS'.
-- rescaledClockToS
--   :: forall m cl time tag timeto. Monad m
--   => RescaledClock cl time timeto -> RescaledClockS m cl time tag timeto tag
-- rescaledClockToS = rescaledClockMToS <<< rescaledClockToM

-- | Applying a monad morphism yields a new clock.
data HoistClock m1 m2 cl = HoistClock  -- cl (m1 ~> m2)
  { unhoistedClock :: cl
  , monadMorphism  :: m1 ~> m2
  }

-- hoistMSF m x = morphS m x

instance hoistClock :: (Monad m1, Monad m2, Clock m1 cl time tag diff)
      => Clock m2 (HoistClock m1 m2 cl) time tag diff where
  -- initClock (HoistClock unhoistedClock monadMorphism) = do
  initClock (HoistClock cl) = do
    (runningClock /\ initialTime) <- cl.monadMorphism (initClock cl.unhoistedClock) 
    -- let
    -- forall a b m1 m2
    --     . Monad m2
    --   =>  Monad m1
    --   => (m1 ~> m2)
    --   -> MSF m1 a b
    --   -> MSF m2 a b
      -- hoistMSF :: MSF m1 Unit (Tuple time tag) -> MSF m2 Unit (Tuple time tag)
    -- TODO Look out for API changes in dunai here
    pure
      ( morphS (cl.monadMorphism) runningClock
      /\ initialTime
      )


-- | Lift a clock type into a monad transformer.
type LiftClock m t cl = HoistClock m (t m) cl

-- -- | Lift a clock value into a monad transformer.
liftClock :: forall m t cl. Monad m => MonadTrans t => cl -> LiftClock m t cl
liftClock unhoistedClock = HoistClock 
  { monadMorphism : lift
  , unhoistedClock : unhoistedClock
  }

-- | Lift a clock type into 'MonadEffect'.
type EffectClock m cl = HoistClock Effect m cl

-- | Lift a clock value into 'MonadEffect'.
effectClock :: forall m cl. MonadEffect m => cl -> EffectClock m cl
effectClock cl = HoistClock
  { monadMorphism : liftEffect
  , unhoistedClock : cl
  }