module Server(
    migrate,
    proxy,
    server,
    app
) where

import Database.Esqueleto.Experimental (SqlBackend, Migration)
import RIO (IO, ReaderT (..), Monad (..))
import RIO.Prelude ((<$>), (<*>))
import Servant (Application, Proxy (..), Server, serve)
import Servant.API ((:<|>) (..))
import Katip (KatipContextT)
import Api (Api)
import qualified Server.User as ServerUser
import qualified Server.Task as ServerTask

migrate :: Migration
migrate = ServerUser.migrate >> ServerTask.migrate

proxy :: Proxy Api
proxy = Proxy

server :: ReaderT SqlBackend (KatipContextT IO) (Server Api)
server = (:<|>) <$> ServerUser.server <*> ServerTask.server

app :: ReaderT SqlBackend (KatipContextT IO) Application
app = serve proxy <$> server
