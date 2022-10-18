{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.User
  ( User (..),
    Api,
  )
where

import Api.Basic (ApiResult)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Text (Text)
import RIO (Eq, Generic, Show)
import Servant.API (Get, JSON, type (:>))

data User = User
  { userId :: Int64,
    userName :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type Api = "users" :> Get '[JSON] (ApiResult [Value])