{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Task
  ( migrate,
    Api,
    server,
  )
where

import Api.Basic (ApiResult (..), FieldId (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, withScientific, withText, (.:), (.:?))
import Data.Maybe (Maybe (..), isJust, isNothing)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto.Experimental (Entity (..), PersistStoreWrite (insertMany_), SqlBackend, delete, entityIdToJSON, from, in_, innerJoin, insert, notIn, on, select, selectOne, set, table, toSqlKey, transactionSave, update, val, valList, where_, (&&.), (=.), (==.), (^.), type (:&) (..))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Katip (Katip (getLogEnv), KatipContext (getKatipContext, getKatipNamespace), KatipContextT, Severity (..), runKatipContextT, showLS)
import Katip.Monadic (logTM)
import RIO (Alternative (..), Bool (..), Either (..), Enum, Eq (..), Generic, IO, Int, Int64, MonadFail (..), MonadReader (ask), ReaderT (..), Show, Traversable (sequence), filter, flip, join, pure, show, (+), (<&>), (<>), Functor)
import RIO.Prelude (return, ($), (.), (<$>), (<*>))
import Servant (Capture, FromHttpApiData, Handler, Post, ReqBody, Server, parseQueryParam)
import Servant.API (Get, JSON, (:<|>) (..), (:>))

share
  [mkPersist sqlSettings, mkMigrate "migrate"]
  [persistLowerCase|
Task
    name Text
    done Bool default=False
    doneAt UTCTime Maybe
    createAt UTCTime
    updateAt UTCTime
    drop Bool default=False
    deriving Eq Show Generic FromJSON ToJSON
TaskFamily
    idFore TaskId
    idRear TaskId
    length Int
    Primary idFore idRear
    deriving Eq Show
|]

type Api =
  "tasks" :> Capture "listType" ListType :> Get '[JSON] (ApiResult [Value])
    :<|> "subTasks" :> Capture "taskId" Int64 :> Get '[JSON] (ApiResult [Value])
    :<|> "task" :> Capture "taskId" Int64 :> Get '[JSON] (ApiResult Value)
    :<|> "task" :> "insert" :> ReqBody '[JSON] ToInsert :> Post '[JSON] (ApiResult Value)
    :<|> "task" :> "update" :> ReqBody '[JSON] ToUpdate :> Post '[JSON] (ApiResult ())
    :<|> "task" :> "delete" :> ReqBody '[JSON] FieldId :> Post '[JSON] (ApiResult ())

server :: ReaderT SqlBackend (KatipContextT IO) (Server Api)
server = do
  backend <- ask
  logEnv <- getLogEnv
  katipCtx <- getKatipContext
  katipNs <- getKatipNamespace
  return $
    runKatipContextT logEnv katipCtx katipNs . flip runReaderT backend <$> listTask
      :<|> (runKatipContextT logEnv katipCtx katipNs . flip runReaderT backend <$> listSubTask)
      :<|> (runKatipContextT logEnv katipCtx katipNs . flip runReaderT backend <$> oneTask)
      :<|> (runKatipContextT logEnv katipCtx katipNs . flip runReaderT backend <$> insertTask)
      :<|> (runKatipContextT logEnv katipCtx katipNs . flip runReaderT backend <$> updateTask)
      :<|> (runKatipContextT logEnv katipCtx katipNs . flip runReaderT backend <$> deleteTask)

data ListType = ListAll | ListTodo | ListDone deriving (Eq, Enum, Show)

instance FromHttpApiData ListType where
  parseQueryParam "all" = Right ListAll
  parseQueryParam "todo" = Right ListTodo
  parseQueryParam "done" = Right ListDone
  parseQueryParam _ = Left "not supported task list type"

listTask :: ListType -> ReaderT SqlBackend (KatipContextT Handler) (ApiResult [Value])
listTask listType = do
  tasks <- select $ do
    task <- from $ table @Task
    case listType of
      ListAll -> return ()
      ListTodo -> where_ (task ^. TaskDone ==. val False)
      ListDone -> where_ (task ^. TaskDone ==. val True)
    pure task
  return . ApiResultSuccess $ entityIdToJSON <$> tasks

listSubTask :: Int64 -> ReaderT SqlBackend (KatipContextT Handler) (ApiResult [Value])
listSubTask idNum = do
  ts <- select $ do
    (tf :& t) <- from $ table @TaskFamily `innerJoin` table @Task `on` (\(tf' :& t') -> tf' ^. TaskFamilyIdRear ==. t' ^. TaskId)
    where_ $ tf ^. TaskFamilyIdFore ==. val (toSqlKey idNum)
    pure t
  pure . ApiResultSuccess $ entityIdToJSON <$> ts

oneTask :: Int64 -> ReaderT SqlBackend (KatipContextT Handler) (ApiResult Value)
oneTask idNum = do
  mEntity <- selectOne $ do
    task <- from $ table @Task
    where_ (task ^. TaskId ==. val (toSqlKey idNum))
    return task
  let result = entityIdToJSON <$> mEntity
  case result of
    Nothing -> pure $ ApiResultFailure 1 "not found"
    Just v -> pure $ ApiResultSuccess v

data ToInsert = ToInsert
  { toInsertParentId :: Maybe Int64,
    toInsertName :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToInsert where
  parseJSON = withObject "ToInsert" $ \v -> ToInsert <$> v .:? "parentId" <*> v .: "name"

insertTask :: ToInsert -> ReaderT SqlBackend (KatipContextT Handler) (ApiResult Value)
insertTask ToInsert {toInsertParentId, toInsertName} = do
  now <- liftIO getCurrentTime
  let task =
        Task
          { taskName = toInsertName,
            taskDone = False,
            taskDoneAt = Nothing,
            taskCreateAt = now,
            taskUpdateAt = now,
            taskDrop = False
          }
  taskKey <- insert task
  case toInsertParentId of
    Nothing -> do
      insertMany_ [TaskFamily taskKey taskKey 0]
    Just parentId -> do
      relaEntities <- select $ do
        tf <- from $ table @TaskFamily
        where_ (tf ^. TaskFamilyIdRear ==. val (toSqlKey parentId))
        pure tf
      let relaRecords = entityVal <$> relaEntities <&> (\(TaskFamily idFore _ pLen) -> TaskFamily idFore taskKey (pLen + 1))
      insertMany_ (TaskFamily taskKey taskKey 0 : relaRecords)
  transactionSave
  return . ApiResultSuccess . entityIdToJSON $ Entity taskKey task

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

updateTask :: ToUpdate -> ReaderT SqlBackend (KatipContextT Handler) (ApiResult ())
updateTask toUpdate@ToUpdate {toUpdateParentId, toUpdateId, toUpdateName, toUpdateDone, toUpdateDrop} = do
  $(logTM) InfoS (showLS toUpdate)
  now <- liftIO getCurrentTime
  let doneAtCond = if isNothing toUpdateDone then Nothing else pure . (TaskDoneAt =.) . val . join $ toUpdateDone <&> (\done -> if done then Just now else Nothing)
  update $ \task -> do
    let mul =
          sequence . filter isJust $
            [ (TaskName =.) . val <$> toUpdateName,
              (TaskDone =.) . val <$> toUpdateDone,
              doneAtCond,
              Just $ TaskUpdateAt =. val now,
              (TaskDrop =.) . val <$> toUpdateDrop
            ]
    case mul of
      Nothing -> return ()
      Just ul -> do
        set task ul
        where_ $ task ^. TaskId ==. val (toSqlKey toUpdateId)
  rearRelaEntities <- select $ do
    tf <- from $ table @TaskFamily
    where_ (tf ^. TaskFamilyIdFore ==. val (toSqlKey toUpdateId))
    pure tf
  let rearIds = taskFamilyIdRear . entityVal <$> rearRelaEntities
  case toUpdateParentId of
    Nothing -> do
      $(logTM) InfoS "parentId don't need to update"
      pure ()
    Just parentId -> do
      $(logTM) InfoS "parentId need to update"
      delete $ do
        tf <- from $ table @TaskFamily
        where_ ((tf ^. TaskFamilyIdRear `in_` valList rearIds) &&. (tf ^. TaskFamilyIdFore `notIn` valList rearIds))
      case parentId of
        ParentIdNull -> do
          $(logTM) InfoS "parentId update to null"
          pure ()
        ParentId pId -> do
          $(logTM) InfoS (showLS $ "parentId update to " <> show pId)
          foreRelaEntities <- select $ do
            tf <- from $ table @TaskFamily
            where_ (tf ^. TaskFamilyIdRear ==. val (toSqlKey pId))
            pure tf
          let relas = [(foreRela, rearRela) | foreRela <- entityVal <$> foreRelaEntities, rearRela <- entityVal <$> rearRelaEntities] <&> (\(TaskFamily idFore _ pLenA, TaskFamily _ idRear pLenB) -> TaskFamily idFore idRear (pLenA + pLenB + 1))
          insertMany_ relas
  transactionSave
  pure $ ApiResultSuccess ()

deleteTask :: FieldId -> ReaderT SqlBackend (KatipContextT Handler) (ApiResult ())
deleteTask FieldId {fieldId} = do
  relaEntities <- select $ do
    tf <- from $ table @TaskFamily
    where_ (tf ^. TaskFamilyIdFore ==. val (toSqlKey fieldId))
    pure tf
  let rearIds = taskFamilyIdRear . entityVal <$> relaEntities
  delete $ do
    tf <- from $ table @TaskFamily
    where_ (tf ^. TaskFamilyIdRear `in_` valList rearIds)
  delete $ do
    task <- from $ table @Task
    where_ (task ^. TaskId `in_` valList rearIds)
  transactionSave
  pure $ ApiResultSuccess ()