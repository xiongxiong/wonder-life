{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api
  ( app,
    migrate,
  )
where

import Database.Esqueleto.Experimental (SqlBackend, Migration)
import RIO (IO, ReaderT (..), Monad (..))
import RIO.Prelude ((<$>), (<*>))
import Servant (Application, Proxy (..), Server, serve)
import Servant.API ((:<|>) (..))
import qualified Api.User as ApiUser
import qualified Api.Task as ApiTask
import Katip (KatipContextT)

migrate :: Migration
migrate = ApiUser.migrate >> ApiTask.migrate

type Api = ApiUser.Api :<|> ApiTask.Api

proxy :: Proxy Api
proxy = Proxy

server :: ReaderT SqlBackend (KatipContextT IO) (Server Api)
server = (:<|>) <$> ApiUser.server <*> ApiTask.server

app :: ReaderT SqlBackend (KatipContextT IO) Application
app = serve proxy <$> server
