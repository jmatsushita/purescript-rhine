{- |
Periodic clocks are defined by a stream of ticks with periodic time differences.
They model subclocks of a fixed reference clock.
The time differences are supplied at the type level.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FRP.Rhine.Clock.Periodic where
-- module FRP.Rhine.Clock.Periodic (Periodic (Periodic)) where

import Control.Monad.Rec.Class (forever)

-- base
import Control.Monad (class Monad)
-- import Data.List.NonEmpty hiding (unfold)
import Data.Maybe (fromMaybe)
-- import GHC.TypeLits (Nat, KnownNat, natVal)

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
-- import FRP.Rhine.Clock.Proxy
import Control.Monad.Schedule

-- import Type.Data.TypeList (class IsNonEmptyTypeListNat)
-- * The 'Periodic' clock

-- | A clock whose tick lengths cycle through
--   a (nonempty) list of type-level natural numbers.
--   E.g. @Periodic '[1, 2]@ ticks at times 1, 3, 4, 5, 7, 8, etc.
--
--   The waiting side effect is formal, in 'ScheduleT'.
--   You can use e.g. 'runScheduleIO' to produce an actual delay.
foreign import data Periodic :: TypeLevelNat -> Type
-- data Periodic (n : ns)

instance clockPeriodic
      :: (Monad m, IsNonEmptyTypeListNat v)
      => Clock (ScheduleT Integer m) (Periodic v) where
  -- type Time (Periodic v) = Integer
  -- type Tag  (Periodic v) = ()
  initClock cl = return
    ( cycleS (theList cl) >>> withSideEffect wait >>> (accumulateWith (+) 0) &&& arr (const unit)
    /\ 0
    )

instance getClockProxyPeriodic :: GetClockProxy (Periodic v)

-- * Type-level trickery to extract the type value from the singleton

-- data HeadClProxy (n :: Nat) where
--   HeadClProxy :: Periodic (n : ns) -> HeadClProxy n

-- headClProxy :: IsNonEmptyTypeListNat l => l -> Int
-- headClProxy = head <<< reflectTypeNonEmptyListNat

headCl :: IsNonEmptyTypeListNat n => Periodic n -> Int
headCl cl = head <<< reflectTypeNonEmptyListNat

tailCl :: Periodic (n1 : n2 : ns) -> Periodic (n2 : ns)
tailCl Periodic = Periodic

-- class NonemptyNatList (v :: [Nat]) where
--   theList :: Periodic v -> NonEmpty Integer

-- instance KnownNat n => NonemptyNatList '[n] where
--   theList cl = headCl cl :| []

-- instance (KnownNat n1, KnownNat n2, NonemptyNatList (n2 : ns))
--       => NonemptyNatList (n1 : n2 : ns) where
--   theList cl = headCl cl <| theList (tailCl cl)


-- * Utilities

-- TODO Port back to dunai when naming issues are resolved
-- | Repeatedly outputs the values of a given list, in order.
cycleS :: Monad m => NonEmpty a -> MSF m Unit a
cycleS as = unfold (second (fromMaybe as) . uncons) as

{-
-- TODO Port back to dunai when naming issues are resolved
delayList :: [a] -> MSF a a
delayList [] = id
delayList (a : as) = delayList as >>> delay a
-}