module Data.Reminder
(
    PeriodUnit(..)
) where

import RIO.Prelude.Types ( Show, Integer, Eq, Enum )
import Data.Time (UTCTime)

-- | TimeUnit ~ Year | Month | Week | Day | Hour | Minute | Second
data TimeUnit = Year | Month | Week | Day | Hour | Minute | Second deriving (Eq, Enum, Show)

-- | OffPoint ~ UTCTime
type OffPoint = UTCTime

-- | ContinuePolicy ~ continue policy
data ContinuePolicy = Instant TimeUnit | Natural deriving (Eq, Show)

-- | PeriodUnit
data PeriodUnit = Every Integer ContinuePolicy OffPoint | Sheet [Integer] deriving Show






