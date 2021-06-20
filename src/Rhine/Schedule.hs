rescaledScheduleS
  :: Monad m
  => Schedule m cl1 cl2
  -> Schedule m (RescaledClockS m cl1 time tag1) (RescaledClockS m cl2 time tag2)
rescaledScheduleS Schedule {..} = Schedule initSchedule'
  where
    initSchedule' (RescaledClockS cl1 rescaleS1) (RescaledClockS cl2 rescaleS2) = do
      (runningSchedule, initTime ) <- initSchedule cl1 cl2
      (rescaling1     , initTime') <- rescaleS1 initTime
      (rescaling2     , _        ) <- rescaleS2 initTime
      let runningSchedule'
            = runningSchedule >>> proc (time, tag12) -> case tag12 of
                Left  tag1 -> do
                  (time', tag1') <- rescaling1 -< (time, tag1)
                  returnA -< (time', Left  tag1')
                Right tag2 -> do
                  (time', tag2') <- rescaling2 -< (time, tag2)
                  returnA -< (time', Right tag2')
      return (runningSchedule', initTime')
