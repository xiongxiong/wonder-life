{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Task
  ( Task (..),
    Api,
    ParentId (..),
    ListType (..),
    ToInsert (..),
    ToUpdate (..),
  )
where

import Api.Basic (ApiResult, FieldId)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, withScientific, withText, (.:), (.:?))
import Data.Aeson.KeyMap (fromList)
import Data.Scientific (scientific, toBoundedInteger)
import Data.Time (UTCTime)
import RIO (Applicative (..), Bool, Either (..), Enum, Eq (..), Generic, Int64, Maybe (..), MonadFail (..), Show, Text, fromIntegral, pure, ($), (<$>), (<|>))
import Servant.API (Capture, FromHttpApiData (..), Get, JSON, Post, ReqBody, ToHttpApiData (..), type (:<|>), type (:>))

data Task = Task
  { taskId :: Int64,
    taskName :: Text,
    taskDone :: Bool,
    taskDoneAt :: Maybe UTCTime,
    taskCreateAt :: UTCTime,
    taskUpdateAt :: UTCTime,
    taskDrop :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> Task <$> v .: "id" <*> v .: "taskName" <*> v .: "taskDone" <*> v .:? "taskDoneAt" <*> v .: "taskCreateAt" <*> v .: "taskUpdateAt" <*> v .: "taskDrop"

instance ToJSON Task where
  toJSON Task {taskId, taskName, taskDone, taskDoneAt, taskCreateAt, taskUpdateAt, taskDrop} =
    Object $
      fromList
        [ ("id", toJSON taskId),
          ("taskName", toJSON taskName),
          ("taskDone", toJSON taskDone),
          ("taskDoneAt", toJSON taskDoneAt),
          ("taskCreateAt", toJSON taskCreateAt),
          ("taskUpdateAt", toJSON taskUpdateAt),
          ("taskDrop", toJSON taskDrop)
        ]

type Api =
  "tasks" :> Capture "listType" ListType :> Get '[JSON] (ApiResult [Value])
    :<|> "subTasks" :> Capture "taskId" Int64 :> Get '[JSON] (ApiResult [Value])
    :<|> "task" :> Capture "taskId" Int64 :> Get '[JSON] (ApiResult Value)
    :<|> "task" :> "insert" :> ReqBody '[JSON] ToInsert :> Post '[JSON] (ApiResult Value)
    :<|> "task" :> "update" :> ReqBody '[JSON] ToUpdate :> Post '[JSON] (ApiResult ())
    :<|> "task" :> "delete" :> ReqBody '[JSON] FieldId :> Post '[JSON] (ApiResult ())

data ParentId = ParentIdNull | ParentId Int64 deriving (Eq, Show)

instance FromJSON ParentId where
  parseJSON =
    (<|>) <$> withText "ParentId" (\v -> if v == "null" then pure ParentIdNull else fail "str cannot parse")
      <*> withScientific
        "ParentId"
        ( \v -> case toBoundedInteger v of
            Nothing -> fail "num cannot parse"
            Just n -> pure $ ParentId n
        )

instance ToJSON ParentId where
  toJSON ParentIdNull = String "null"
  toJSON (ParentId n) = Number $ scientific (fromIntegral n) 0

data ListType = ListAll | ListTodo | ListDone | ListDrop deriving (Eq, Enum, Show)

instance FromHttpApiData ListType where
  parseQueryParam "all" = Right ListAll
  parseQueryParam "todo" = Right ListTodo
  parseQueryParam "done" = Right ListDone
  parseQueryParam "drop" = Right ListDrop
  parseQueryParam _ = Left "not supported task list type"

instance ToHttpApiData ListType where
  toQueryParam ListAll = "all"
  toQueryParam ListTodo = "todo"
  toQueryParam ListDone = "done"
  toQueryParam ListDrop = "drop"

data ToInsert = ToInsert
  { toInsertParentId :: Maybe Int64,
    toInsertName :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToInsert where
  parseJSON = withObject "ToInsert" $ \v -> ToInsert <$> v .:? "parentId" <*> v .: "name"

instance ToJSON ToInsert where
  toJSON ToInsert {toInsertParentId, toInsertName} =
    Object $
      fromList
        [ ("parentId", toJSON toInsertParentId),
          ("name", toJSON toInsertName)
        ]

data ToUpdate = ToUpdate
  { toUpdateParentId :: Maybe ParentId,
    toUpdateId :: Int64,
    toUpdateName :: Maybe Text,
    toUpdateDone :: Maybe Bool,
    toUpdateDrop :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToUpdate where
  parseJSON = withObject "ToUpdate" $ \v -> ToUpdate <$> v .:? "parentId" <*> v .: "id" <*> v .:? "name" <*> v .:? "done" <*> v .:? "drop"

instance ToJSON ToUpdate where
  toJSON ToUpdate {toUpdateParentId, toUpdateId, toUpdateName, toUpdateDone, toUpdateDrop} =
    Object $
      fromList
        [ ("parentId", toJSON toUpdateParentId),
          ("id", toJSON toUpdateId),
          ("name", toJSON toUpdateName),
          ("done", toJSON toUpdateDone),
          ("drop", toJSON toUpdateDrop)
        ]