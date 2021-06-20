{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Clock.Proxy where

-- base

import FRP.Rhine.Clock
import FRP.Rhine.Schedule

-- import Data.Kind (Type)
import Data.Leibniz

-- data Expr a where
--   Val :: Int -> Expr Int
--   Add :: Expr Int -> Expr Int -> Expr Int
--   Mult :: Expr Int -> Expr Int -> Expr Int
--   Equal :: Expr Int -> Expr Int -> Expr Bool
--   Not :: Expr Bool -> Expr Bool
-- eval :: Expr a -> a
-- eval (Val x) = x
-- eval (Add x y) = eval x + eval y
-- eval (Mult x y) = eval x * eval y
-- eval (Equal x y) = eval x == eval y
-- eval (Not x) = not (eval x)

-- data Expr a where
--     Val     :: Int                     -> Expr Int
--     Lambda  :: (Expr a -> Expr b)      -> Expr (Expr a -> Expr b)
--     Apply   :: Expr (Expr a -> Expr b) -> Expr a -> Expr b
--     Add     :: Expr Int -> Expr Int    -> Expr Int

-- class LambdaSym repr where 
--   val :: Int -> repr Int 
--   lambda :: forall a b. (repr a -> repr b) -> repr (a -> b) 
--   apply :: forall a b. repr (a -> b) -> repr a -> repr b

-- add :: repr Int -> repr Int -> repr Int

-- data R a = R a

-- eval :: forall a. R a -> a
-- eval (R a) = a

-- instance lambdaSymmR :: LambdaSym R where 
--   val a = R a 
--   lambda f = R (\a -> unR $ f (R a)) 
--   apply (R f) (R a) = R (f a)
  
-- add (R x) (R y) = R $ x + y

-- class MultiplySymm repr where 
--   mult :: repr Int -> repr Int -> repr Int 
  
-- testR' :: forall repr. LambdaSym repr => MultiplySymm repr => repr Int
-- testR' = apply (lambda (\x -> mult x x)) (val 10)

-- Try to transform to Leibniz equality (don't think Tagless final is needed)
-- https://medium.com/@hgiasac/purescript-gadts-alternatives-recap-7960daf4acd8

-- data ClockProxy cl = ClockProxy cl

-- class LeafProxy cp where
--   leaf :: TypeEquals cp (In cp) => TypeEq)

-- | Witnesses the structure of a clock type,
--   in particular whether 'SequentialClock's or 'ParallelClock's are involved.
data ClockProxy cl 
  = LeafProxy (cl ~ In cl) (cl ~ Out cl) 
  | SequentialProxy (ClockProxy cl1) (ClockProxy cl2) (cl ~ SequentialClock m cl1 cl2)
  | ParallelProxy (ClockProxy clL) (ClockProxy clR) (cl ~ ParallelClock m clL clR)

inProxy :: ClockProxy cl -> ClockProxy (In cl)
inProxy LeafProxy = LeafProxy
inProxy (SequentialProxy p1 p2) = inProxy p1
inProxy (ParallelProxy pL pR) = ParallelProxy (inProxy pL) (inProxy pR)

outProxy :: ClockProxy cl -> ClockProxy (Out cl)
outProxy LeafProxy = LeafProxy
outProxy (SequentialProxy p1 p2) = outProxy p2
outProxy (ParallelProxy pL pR) = ParallelProxy (outProxy pL) (outProxy pR)

-- | Return the incoming tag, assuming that the incoming clock is ticked,
--   and 'Nothing' otherwise.
inTag :: ClockProxy cl -> Tag cl -> Maybe (Tag (In cl))
inTag (SequentialProxy p1 _) (Left  tag1) = inTag p1 tag1
inTag (SequentialProxy _  _) (Right _)    = Nothing
inTag (ParallelProxy pL _) (Left  tagL) = Left  <$> inTag pL tagL
inTag (ParallelProxy _ pR) (Right tagR) = Right <$> inTag pR tagR
inTag LeafProxy tag = Just tag

-- | Return the incoming tag, assuming that the outgoing clock is ticked,
--   and 'Nothing' otherwise.
outTag :: ClockProxy cl -> Tag cl -> Maybe (Tag (Out cl))
outTag (SequentialProxy _ _ ) (Left  _)    = Nothing
outTag (SequentialProxy _ p2) (Right tag2) = outTag p2 tag2
outTag (ParallelProxy pL _) (Left  tagL) = Left  <$> outTag pL tagL
outTag (ParallelProxy _ pR) (Right tagR) = Right <$> outTag pR tagR
outTag LeafProxy tag = Just tag

-- TODO Should this be a superclass with default implementation of clocks? But then we have a circular dependency...
-- No we don't, Schedule should not depend on clock (the type).
-- | Clocks should be able to automatically generate a proxy for themselves.
class GetClockProxy cl where
  getClockProxy :: ClockProxy cl

  -- default getClockProxy
  --   :: (cl ~ In cl, cl ~ Out cl)
  --   => ClockProxy cl
  -- getClockProxy = LeafProxy

instance seqClockGet :: (GetClockProxy cl1, GetClockProxy cl2) => GetClockProxy (SequentialClock m cl1 cl2) where
  getClockProxy = SequentialProxy getClockProxy getClockProxy

instance parClockGet :: (GetClockProxy cl1, GetClockProxy cl2) => GetClockProxy (ParallelClock m cl1 cl2) where
  getClockProxy = ParallelProxy getClockProxy getClockProxy

instance hoistClockGet :: GetClockProxy cl => GetClockProxy (HoistClock m1 m2 cl)
instance rescaleClockGet :: GetClockProxy cl => GetClockProxy (RescaledClock cl time)
instance rescaledMClockGet :: GetClockProxy cl => GetClockProxy (RescaledClockM m cl time)
instance rescaledSClockGet ::GetClockProxy cl => GetClockProxy (RescaledClockS m cl time tag)

-- | Extract a clock proxy from a type.
class ToClockProxy a cl where
  -- type Cl a :: Type

  toClockProxy :: a -> ClockProxy cl

  -- default toClockProxy
  --   :: GetClockProxy (Cl a)
  --   => a -> ClockProxy (Cl a)
  -- toClockProxy _ = getClockProxy
