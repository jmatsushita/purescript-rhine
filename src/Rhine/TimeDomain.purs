{- |
This module defines the 'TimeDomain' class.
Its instances model time.
Several instances such as 'UTCTime', 'Double' and 'Integer' are supplied here.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.TimeDomain
  ( module FRP.Rhine.TimeDomain
  , module Data.DateTime
  )
  where

import Prelude ((-), ($))
import Data.Unit
import Data.Num
import Data.Eq
import Data.Semiring
import Data.Ring
import Data.CommutativeRing

import Data.DateTime (DateTime(..), diff)
import Data.Time.Duration (Milliseconds(..))

-- | A time domain is an affine space representing a notion of time,
--   such as real time, simulated time, steps, or a completely different notion.
-- type Diff time

class TimeDomain time diff | time -> diff where
  diffTime :: time -> time -> diff


instance utcTimeDomain :: TimeDomain DateTime Milliseconds where
  diffTime t1 t2 = diff t1 t2

instance numberTimeDomain :: TimeDomain Number Number where
  diffTime = (-)

instance intTimeDomain :: TimeDomain Int Int where
  diffTime          = (-)

instance unitTimeDomain :: TimeDomain Unit Unit where
  diffTime _ _ = unit

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain a

fromNumTimeDomain :: forall a. NumTimeDomain a -> a
fromNumTimeDomain (NumTimeDomain a) = a

derive newtype instance numTimeDomainEq :: Eq a => Eq (NumTimeDomain a)
derive newtype instance numTimeDomainSemiring :: Semiring a => Semiring (NumTimeDomain a)
derive newtype instance numTimeDomainRing :: Ring a => Ring (NumTimeDomain a)
derive newtype instance numTimeDomainCommutativeRing :: CommutativeRing a => CommutativeRing (NumTimeDomain a)
derive newtype instance numTimeDomainNum :: Num a => Num (NumTimeDomain a)

-- instance numTimeDomainNum :: Num a => Num (NumTimeDomain a) where
--   negate = negate
--   abs = abs
--   signum = signum
--   fromBigInt = fromBigInt

instance numTimeDomain :: Num a => TimeDomain (NumTimeDomain a) (NumTimeDomain a) where
  diffTime = (-)