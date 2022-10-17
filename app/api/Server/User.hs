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

module Server.User
  ( migrate,
    server,
  )
where

import Api.Basic (ApiResult (..))
import Api.User (Api)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Text (Text)
import Database.Esqueleto.Experimental (SqlBackend, entityIdToJSON, from, select, table)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Katip (Katip (getLogEnv), KatipContext (getKatipContext, getKatipNamespace), KatipContextT, runKatipContextT)
import RIO (Eq, Generic, IO, MonadReader (ask), ReaderT (..), Show)
import RIO.Prelude (return, ($), (.), (<$>))
import Servant (Handler, Server)

share
  [mkPersist sqlSettings, mkMigrate "migrate"]
  [persistLowerCase|
User
    name Text
    deriving Eq Show Generic FromJSON ToJSON
|]

server :: ReaderT SqlBackend (KatipContextT IO) (Server Api)
server = do
  backend <- ask
  logEnv <- getLogEnv
  katipCtx <- getKatipContext
  katipNs <- getKatipNamespace
  return . runKatipContextT logEnv katipCtx katipNs . runReaderT listUser $ backend

listUser :: ReaderT SqlBackend (KatipContextT Handler) (ApiResult [Value])
listUser = do
  entities <- select . from $ table @User
  return . ApiResultSuccess $ entityIdToJSON <$> entities
