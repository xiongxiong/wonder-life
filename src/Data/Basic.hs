{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Basic
(
    Plan,
    createTask,
    removeTask,
    Task(..),
    newTask,
    doneTask,
    undoneTask
) where

import RIO
import Data.Time.Clock ( UTCTime, getCurrentTime )
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Data.Aeson (FromJSON, ToJSON)

textUuid :: IO Text
textUuid = fmap (T.pack . UUID.toString) UUIDv4.nextRandom

type Plan = [Task]

createTask :: Plan -> Task -> Plan
createTask plan task = task : plan

removeTask :: Plan -> Task -> Plan
removeTask plan Task{taskId = toRemove} = filter (\Task{taskId = toCheck} -> toCheck /= toRemove) plan

---------------------------------------------------------------------------------------------------

data Task = Task {
    taskId :: Text, -- 任务标识
    title :: Text, -- 任务标题
    detail :: Text, -- 任务详情
    status :: TaskStatus, -- 任务状态
    timer :: Maybe TaskTimer, -- 任务计时器
    createAt :: UTCTime, -- 创建时间
    updateAt :: UTCTime, -- 更新时间
    subTasks :: [Task] -- 子任务
} deriving (Generic, Show)

instance FromJSON Task

instance ToJSON Task

-- 创建任务
newTask :: Text -> Text -> IO Task
newTask title detail = do
    current <- getCurrentTime
    taskId <- textUuid
    return Task{
        taskId,
        title,
        detail,
        status = Todo,
        timer = Nothing,
        createAt = current,
        updateAt = current,
        subTasks = []
    }

-- 完成任务
doneTask :: Task -> IO Task
doneTask task = getCurrentTime >>= (\current -> return task{status = Done current})

-- 重置任务
undoneTask :: Task -> IO Task
undoneTask task = getCurrentTime >>= (\current -> return task{status = Todo, updateAt = current})

---------------------------------------------------------------------------------------------------

data TaskStatus = Todo | Done UTCTime deriving (Generic, Show)

instance FromJSON TaskStatus

instance ToJSON TaskStatus

---------------------------------------------------------------------------------------------------

data TaskTimer = TaskTimer {
    startAt :: Maybe UTCTime,
    totalUse :: Integer
} deriving (Generic, Show)

instance FromJSON TaskTimer

instance ToJSON TaskTimer

---------------------------------------------------------------------------------------------------