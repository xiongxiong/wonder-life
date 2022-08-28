{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Basic
(
    Plan,
    createTask,
    removeTask,
    Task(..),
    TaskStatus(..),
    newTask,
    doneTask,
    undoneTask,
    taskIsDone,
    taskIsTodo
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
    taskTitle :: Text, -- 任务标题
    taskDetail :: Text, -- 任务详情
    taskStatus :: TaskStatus, -- 任务状态
    taskTimer :: Maybe TaskTimer, -- 任务计时器
    taskCreateAt :: UTCTime, -- 创建时间
    taskUpdateAt :: UTCTime, -- 更新时间
    taskSubTasks :: [Task] -- 子任务
} deriving (Generic, Show, Eq)

instance FromJSON Task

instance ToJSON Task

-- 创建任务
newTask :: Text -> Text -> IO Task
newTask taskTitle taskDetail = do
    current <- getCurrentTime
    taskId <- textUuid
    return Task{
        taskId,
        taskTitle,
        taskDetail,
        taskStatus = Todo,
        taskTimer = Nothing,
        taskCreateAt = current,
        taskUpdateAt = current,
        taskSubTasks = []
    }

-- 完成任务
doneTask :: Task -> IO Task
doneTask task = getCurrentTime >>= (\current -> return task{taskStatus = Done current})

-- 重置任务
undoneTask :: Task -> IO Task
undoneTask task = getCurrentTime >>= (\current -> return task{taskStatus = Todo, taskUpdateAt = current})

-- 任务是否已办
taskIsDone :: Task -> Bool
taskIsDone Task{taskStatus = Done _} = True
taskIsDone _ = False

-- 任务是否待办
taskIsTodo :: Task -> Bool
taskIsTodo = not . taskIsDone

---------------------------------------------------------------------------------------------------

data TaskStatus = Todo | Done UTCTime deriving (Generic, Show, Eq)

instance FromJSON TaskStatus

instance ToJSON TaskStatus

---------------------------------------------------------------------------------------------------

data TaskTimer = TaskTimer {
    startAt :: Maybe UTCTime,
    totalUse :: Integer
} deriving (Generic, Show, Eq)

instance FromJSON TaskTimer

instance ToJSON TaskTimer

---------------------------------------------------------------------------------------------------

data TaskPlatView = TaskPlatView {
    taskPlatViewId :: Text, -- 任务标识
    taskPlatViewParentId :: Maybe Text, -- 父任务标识
    taskPlatViewTitle :: Text, -- 任务标题
    taskPlatViewDetail :: Text, -- 任务详情
    taskPlatViewTaskStatus :: TaskStatus, -- 任务状态
    taskPlatViewTaskTimer :: Maybe TaskTimer, -- 任务计时器
    taskPlatViewCreateAt :: UTCTime, -- 创建时间
    taskPlatViewUpdateAt :: UTCTime, -- 更新时间
    taskPlatViewsubTasks :: [Task] -- 子任务
} deriving (Generic, Show, Eq)