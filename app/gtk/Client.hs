module Client(
    users,
    tasks,
    subTasks,
    oneTask,
    insertTask,
    updateTask,
    deleteTask
) where

import Data.Proxy (Proxy (..))
import Api (Api)
import Servant.API ((:<|>)(..))
import Servant.Client (client, ClientM)
import Api.Basic (ApiResult, FieldId (..))
import Data.Aeson (Value)
import Api.Task (ListType, ToInsert (..), ToUpdate (..))
import RIO (Int64)

proxy :: Proxy Api
proxy = Proxy

users :: ClientM (ApiResult [Value])
tasks :: ListType -> ClientM (ApiResult [Value])
subTasks :: Int64 -> ClientM (ApiResult [Value])
oneTask :: Int64 -> ClientM (ApiResult Value)
insertTask :: ToInsert -> ClientM (ApiResult Value)
updateTask :: ToUpdate -> ClientM (ApiResult ())
deleteTask :: FieldId -> ClientM (ApiResult ())
users :<|> (tasks :<|> subTasks :<|> oneTask :<|> insertTask :<|> updateTask :<|> deleteTask) = client proxy