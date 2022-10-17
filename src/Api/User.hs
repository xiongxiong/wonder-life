{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Api.User(
    Api
) where

import Servant.API (type (:>), Get, JSON)
import Api.Basic (ApiResult)
import Data.Aeson (Value)

type Api = "users" :> Get '[JSON] (ApiResult [Value])