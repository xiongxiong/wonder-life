module Client(
    tasks,
    newTask
) where

import Data.Proxy (Proxy (..))
import Api (Api)
import Servant.API ((:<|>)(..))
import Servant.Client (client, ClientM, ClientEnv, runClientM)
import Api.Basic (ApiResult (..), FieldId (..))
import Data.Aeson (Value, Result (..), fromJSON)
import Api.Task (ListType (..), ToInsert (..), ToUpdate (..), Task)
import RIO (Int64, ReaderT, IO, Either (..), Text, ask, MonadIO (..), ($), pure, (.), show, sequence, (<$>))
import Data.Text (pack)

proxy :: Proxy Api
proxy = Proxy

users' :: ClientM (ApiResult [Value])
tasks' :: ListType -> ClientM (ApiResult [Value])
subTasks' :: Int64 -> ClientM (ApiResult [Value])
oneTask' :: Int64 -> ClientM (ApiResult Value)
insertTask' :: ToInsert -> ClientM (ApiResult Value)
updateTask' :: ToUpdate -> ClientM (ApiResult ())
deleteTask' :: FieldId -> ClientM (ApiResult ())
users' :<|> (tasks' :<|> subTasks' :<|> oneTask' :<|> insertTask' :<|> updateTask' :<|> deleteTask') = client proxy

jsonToTask :: Value -> Either Text Task
jsonToTask json = case fromJSON json of
    Success o -> Right o
    Error e -> Left . pack $ e

tasks :: ListType -> ReaderT ClientEnv IO (Either Text [Task])
tasks listType = do
  env <- ask
  res <- liftIO $ runClientM (tasks' listType) env
  case res of
    Left err -> pure . Left . pack . show $ err
    Right (ApiResultSuccess ts) -> pure . sequence $ jsonToTask <$> ts
    Right (ApiResultFailure _ msg) -> pure . Left $ msg
    

newTask :: ToInsert -> ReaderT ClientEnv IO (Either Text Task)
newTask toInsert = do
    env <- ask
    res <- liftIO $ runClientM (insertTask' toInsert) env
    case res of
        Left err -> pure . Left . pack . show $ err
        Right (ApiResultSuccess t) -> pure $ jsonToTask t
        Right (ApiResultFailure _ msg) -> pure $ Left msg