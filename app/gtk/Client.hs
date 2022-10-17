module Client(

) where

import Data.Proxy (Proxy (..))
import Api (Api)
import Servant.API ((:<|>)(..))
import Servant.Client (client)

proxy :: Proxy Api
proxy = Proxy

users :<|> (tasks :<|> subTasks :<|> oneTask :<|> insertTask :<|> updateTask :<|> deleteTask) = client proxy