module Data.Task
(
    User(..),
    Task(..),
) where

import Data.Text (Text)
import Data.Time (UTCTime)
import RIO (Integer, Bool)

data User = User {
    userId :: Integer,
    userName :: Text
}

data Task = Task {
    taskId :: Integer,
    taskName :: Text,
    taskDone :: Bool,
    taskDoneAt :: UTCTime,
    taskCreateAt :: UTCTime
}
