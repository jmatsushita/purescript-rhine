module FRP.Rhine.Schedule.Util where

import Prelude
import Data.Either
import Data.Tuple.Nested

import Data.Profunctor

-- dunai
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Async

-- | In a composite running clock,
--   duplicate the tick of one subclock.
duplicateSubtick :: forall m time a b. Monad m => MSF m Unit (time /\ Either a b) -> MSF m Unit (time /\ Either a (Either a b))
duplicateSubtick runningClock = concatS $ runningClock >>> (arr ?duplicateLeft)
  where
    duplicateLeft (time /\ (Left a))  = [(time /\ Left a) /\ (time /\ (Right $ Left a))]
    duplicateLeft (time /\ (Right b)) = [(time /\ (Right $ Right b))]

-- TODO Why is stuff like this not in base? Maybe send pull request...
swapEither :: forall a b. Either a b -> Either b a
swapEither (Left  a) = Right a
swapEither (Right a) = Left a