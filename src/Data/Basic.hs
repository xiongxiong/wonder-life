{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Basic
  ( Plan,
    createTask,
    removeTask,
    planToTaskPlatView,
    planFromTaskPlatView,
    TaskKind (..),
    Task (..),
    TaskStatus (..),
    newTask,
    taskToTaskPlatView,
    subTask,
    TaskPlatView (..),
    newTaskPlatView
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import RIO
import RIO.List.Partial (last)
import Data.List.Extra ( snoc, init )

textUuid :: IO Text
textUuid = fmap (T.pack . UUID.toString) UUIDv4.nextRandom

type Plan = [Task]

createTask :: Plan -> Task -> Plan
createTask plan task = task : plan

removeTask :: Plan -> Task -> Plan
removeTask plan Task {taskId = toRemove} = filter (\Task {taskId = toCheck} -> toCheck /= toRemove) plan

planToTaskPlatView :: Plan -> [TaskPlatView]
planToTaskPlatView = foldMap (taskToTaskPlatView [])

planFromTaskPlatView :: [TaskPlatView] -> Plan
planFromTaskPlatView views = toPlan views []
  where
    toPlan :: [TaskPlatView] -> Plan -> Plan
    toPlan [] plan = plan
    toPlan (taskPlatView : rViews) [] = toPlan rViews [taskPlatViewToBareTask taskPlatView]
    toPlan (taskPlatView@TaskPlatView {taskPlatViewAncestors} : rViews) plan =
      if null taskPlatViewAncestors
        then toPlan rViews (plan `snoc` taskPlatViewToBareTask taskPlatView)
        else toPlan rViews (init plan `snoc` hook taskPlatView 1 (last plan))
    hook :: TaskPlatView -> Int -> Task -> Task
    hook taskPlatView@TaskPlatView {taskPlatViewAncestors} layer task@Task{taskSubTasks}
      | aLayer == layer = task{taskSubTasks = taskSubTasks `snoc` taskPlatViewToBareTask taskPlatView}
      | otherwise = task{taskSubTasks = init taskSubTasks `snoc` hook taskPlatView (layer + 1) (last taskSubTasks)}
      where aLayer = length taskPlatViewAncestors
---------------------------------------------------------------------------------------------------

class TaskKind a where
  {-# MINIMAL done, undone, isDone, isTodo #-}

  -- 完成
  done :: a -> IO a

  -- 重置
  undone :: a -> IO a

  -- 是否已办
  isDone :: a -> Bool

  -- 是否待办
  isTodo :: a -> Bool

---------------------------------------------------------------------------------------------------

data Task = Task
  { taskId :: Text, -- 任务标识
    taskTitle :: Text, -- 任务标题
    taskDetail :: Text, -- 任务详情
    taskStatus :: TaskStatus, -- 任务状态
    taskTimer :: Maybe TaskTimer, -- 任务计时器
    taskCreateAt :: UTCTime, -- 创建时间
    taskUpdateAt :: UTCTime, -- 更新时间
    taskSubTasks :: [Task] -- 子任务
  }
  deriving (Generic, Show, Eq)

instance FromJSON Task

instance ToJSON Task

instance TaskKind Task where
  done task = getCurrentTime >>= (\current -> return task {taskStatus = TaskStatusDone current})

  undone task = getCurrentTime >>= (\current -> return task {taskStatus = TaskStatusTodo, taskUpdateAt = current})

  isDone Task {taskStatus = TaskStatusDone _} = True
  isDone _ = False

  isTodo Task {taskStatus = TaskStatusDone _} = True
  isTodo _ = False

-- 创建任务
newTask :: Text -> Text -> IO Task
newTask taskTitle taskDetail = do
  current <- getCurrentTime
  taskId <- textUuid
  return
    Task
      { taskId,
        taskTitle,
        taskDetail,
        taskStatus = TaskStatusTodo,
        taskTimer = Nothing,
        taskCreateAt = current,
        taskUpdateAt = current,
        taskSubTasks = []
      }

taskToTaskPlatView :: [(Text, Text)] -> Task -> [TaskPlatView]
taskToTaskPlatView ancestors Task {..} =
  let platView =
        TaskPlatView
          { taskPlatViewId = taskId,
            taskPlatViewAncestors = ancestors,
            taskPlatViewTitle = taskTitle,
            taskPlatViewDetail = taskDetail,
            taskPlatViewTaskStatus = taskStatus,
            taskPlatViewTaskTimer = taskTimer,
            taskPlatViewCreateAt = taskCreateAt,
            taskPlatViewUpdateAt = taskUpdateAt
          }
      subPlatViews = foldMap (taskToTaskPlatView (ancestors ++ [(taskId, taskTitle)])) taskSubTasks
   in platView : subPlatViews

subTask :: Task -> Task -> Task
subTask parent@Task{taskSubTasks} child = parent{taskSubTasks = taskSubTasks `snoc` child}

---------------------------------------------------------------------------------------------------

data TaskStatus = TaskStatusTodo | TaskStatusDone UTCTime deriving (Generic, Show, Eq)

instance FromJSON TaskStatus

instance ToJSON TaskStatus

---------------------------------------------------------------------------------------------------

data TaskTimer = TaskTimer
  { startAt :: Maybe UTCTime,
    totalUse :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromJSON TaskTimer

instance ToJSON TaskTimer

---------------------------------------------------------------------------------------------------

data TaskPlatView = TaskPlatView
  { taskPlatViewId :: Text, -- 任务标识
    taskPlatViewAncestors :: [(Text, Text)], -- 祖先任务栈 [(任务ID，缩略任务标题)]
    taskPlatViewTitle :: Text, -- 任务标题
    taskPlatViewDetail :: Text, -- 任务详情
    taskPlatViewTaskStatus :: TaskStatus, -- 任务状态
    taskPlatViewTaskTimer :: Maybe TaskTimer, -- 任务计时器
    taskPlatViewCreateAt :: UTCTime, -- 创建时间
    taskPlatViewUpdateAt :: UTCTime -- 更新时间
  }
  deriving (Generic, Show, Eq)

instance TaskKind TaskPlatView where
  done taskPlatView = getCurrentTime >>= (\current -> return taskPlatView {taskPlatViewTaskStatus = TaskStatusDone current})

  undone taskPlatView = getCurrentTime >>= (\current -> return taskPlatView {taskPlatViewTaskStatus = TaskStatusTodo, taskPlatViewUpdateAt = current})

  isDone TaskPlatView {taskPlatViewTaskStatus = TaskStatusDone _} = True
  isDone _ = False

  isTodo TaskPlatView {taskPlatViewTaskStatus = TaskStatusTodo} = True
  isTodo _ = False

-- 创建任务
newTaskPlatView :: [(Text, Text)] -> Text -> Text -> IO TaskPlatView
newTaskPlatView ancestors taskTitle taskDetail = do
  current <- getCurrentTime
  taskId <- textUuid
  return
    TaskPlatView
      { taskPlatViewId = taskId,
        taskPlatViewAncestors = ancestors,
        taskPlatViewTitle = taskTitle,
        taskPlatViewDetail = taskDetail,
        taskPlatViewTaskStatus = TaskStatusTodo,
        taskPlatViewTaskTimer = Nothing,
        taskPlatViewCreateAt = current,
        taskPlatViewUpdateAt = current
      }

taskPlatViewToBareTask :: TaskPlatView -> Task
taskPlatViewToBareTask TaskPlatView {..} =
  Task
    { taskId = taskPlatViewId,
      taskTitle = taskPlatViewTitle,
      taskDetail = taskPlatViewDetail,
      taskStatus = taskPlatViewTaskStatus,
      taskTimer = taskPlatViewTaskTimer,
      taskCreateAt = taskPlatViewCreateAt,
      taskUpdateAt = taskPlatViewUpdateAt,
      taskSubTasks = []
    }