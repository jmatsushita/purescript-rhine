{- |
This module supplies a general purpose monad transformer
that adds a syntactical "delay", or "waiting" side effect.
This allows for universal and deterministic scheduling of clocks
that implement their waiting actions in 'ScheduleT'.
See 'FRP.Rhine.Schedule.Trans' for more details.
-}

{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Schedule where

import Prelude

import Data.Either
import Data.Num
import Data.Tuple.Nested
import Effect.Class

import Control.Monad.Free (wrap)
import Control.Monad.Free.Trans (freeT, FreeT(..), resume, runFreeT, liftFreeT)
import Data.Time.Duration (class Duration, fromDuration)
import Effect.Timer (setTimeout)


-- TODO Implement Time via StateT

{- |
A functor implementing a syntactical "waiting" action.
* 'diff' represents the duration to wait.
* 'a' is the encapsulated value.
-}
data Wait diff a = Wait diff a

instance waitFunctor :: Functor (Wait diff) where
  map f (Wait diff a) = Wait diff (f a)

{- |
Values in @ScheduleT diff m@ are delayed computations with side effects in 'm'.
Delays can occur between any two side effects, with lengths specified by a 'diff' value.
These delays don't have any semantics, it can be given to them with 'runScheduleT'.
-}
type ScheduleT diff = FreeT (Wait diff)

-- | The side effect that waits for a specified amount.
wait :: forall m diff. Monad m => diff -> ScheduleT diff m Unit
wait diff = liftFreeT (Wait diff unit) -- Not sure why this doesn't work

iterT :: forall f m a. Functor f => Monad m => (f (m a) -> m a) -> FreeT f m a -> m a
iterT f m = do
    val <- m
    case map (iterT f) (val) of
        Left x -> pure x
        Right y -> f y

-- | Supply a semantic meaning to 'Wait'.
--   For every occurrence of @Wait diff@ in the @ScheduleT diff m a@ value,
--   a waiting action is executed, depending on 'diff'.
runScheduleT :: Monad m => (diff -> m ()) -> ScheduleT diff m a -> m a
runScheduleT waitAction = iterT $ \(Wait n ma) -> waitAction n >>= \_ -> ma

-- | Run a 'ScheduleT' value in a 'MonadIO',
--   interpreting the times as milliseconds.
runScheduleIO
  :: MonadEffect m
  => Duration a 
  => ScheduleT n m a -> m a
runScheduleIO = runScheduleT $ liftEffect <<< setTimeout <<< (_ * 1000) <<< fromDuration

-- -- TODO The definition and type signature are both a mouthful. Is there a simpler concept?
-- -- | Runs two values in 'ScheduleT' concurrently
-- --   and returns the first one that yields a value
-- --   (defaulting to the first argument),
-- --   and a continuation for the other value.
-- race
--   :: forall diff m a b
--    . Ord diff 
--   => Num diff 
--   => Monad m
--   => ScheduleT    diff m a 
--   -> ScheduleT                            diff m b
--   -> ScheduleT    diff m (Either
--        (                 a /\   ScheduleT diff m b)
--        (ScheduleT diff m a /\                    b)
--      )
-- race ma mb = freeT $ do
--   -- Perform the side effects to find out how long each 'ScheduleT' values need to wait.
--   aWait <- resume ma
--   bWait <- resume mb
--   case aWait of
--     -- 'a' doesn't need to wait. Return immediately and leave the continuation for 'b'.
--     Left a -> pure $ Left $ Left (a /\ freeT (\_ -> pure bWait))
--     -- 'a' needs to wait, so we need to inspect 'b' as well and see which one needs to wait longer.
--     Right (Wait aDiff aCont) -> case bWait of
--     -- 'b' doesn't need to wait. Return immediately and leave the continuation for 'a'.
--       Left b -> pure $ Left $ Right (wait aDiff >>= \_ -> pure $ aCont /\ b)
--       -- Both need to wait. Which one needs to wait longer?
--       Right (Wait bDiff bCont) -> 
--         if aDiff <= bDiff
--         -- 'a' yields first, or both are done simultaneously.
--         then resume do
--           pure $ ?wat
--           -- Perform the wait action that we've deconstructed
--           -- _ <- wait aDiff
--           -- Recurse, since more wait actions might be hidden in 'a' and 'b'. 'b' doesn't need to wait as long, since we've already waited for 'aDiff'.
--           -- pure $ race aCont (wait (bDiff - aDiff) >>= \_ -> bCont)
--         -- 'b' yields first. Analogously.
--         else resume $ do
--           -- _ <- wait bDiff
--           -- pure $ race (wait (aDiff - bDiff) >>= \_ -> aCont) bCont
--           pure ?say

-- -- | Runs both schedules concurrently and returns their results at the end.
-- async
--   :: Ord diff 
--   => Num diff
--   => Monad m
--   => ScheduleT diff m  a -> ScheduleT diff m b
--   -> ScheduleT diff m (a /\                  b)
-- async aSched bSched = do
--   ab <- race aSched bSched
--   case ab of
--     Left  (a /\ bCont) -> do
--       b <- bCont
--       pure (a /\ b)
--     Right (aCont /\ b) -> do
--       a <- aCont
--       pure (a /\ b)