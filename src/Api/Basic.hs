{-# LANGUAGE DeriveGeneric #-}
module Api.Basic
(   ApiResult(..),
    FieldId(..)
) where

import RIO (Int, Text, Int64, Eq, Show, Generic, ($), fromIntegral, (<$>), Maybe (..), (<*>))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (..), withObject, (.:))
import Data.Aeson.KeyMap (fromList, lookup)
import Data.Scientific (scientific)
import Control.Monad (MonadFail(..))

data ApiResult a = ApiResultSuccess a | ApiResultFailure Int Text

instance FromJSON a => FromJSON (ApiResult a) where
  parseJSON = withObject "ApiResult" $ \v -> case lookup "code" v of
    Just (Number 0) -> ApiResultSuccess <$> v .: "data"
    Just (Number _) -> ApiResultFailure <$> v .: "code" <*> v .: "message"
    _ -> fail "ApiResult -- parse json error"

instance ToJSON a => ToJSON (ApiResult a) where
  toJSON (ApiResultSuccess value) = Object $ fromList [("code", Number 0), ("data", toJSON value)]
  toJSON (ApiResultFailure code message) = Object $ fromList [("code", Number $ scientific (fromIntegral code) 0), ("message", String message)]

newtype FieldId = FieldId {fieldId :: Int64} deriving (Eq, Show, Generic)

instance FromJSON FieldId where
  parseJSON = withObject "FieldId" $ \v -> FieldId <$> v .: "id"

instance ToJSON FieldId where
  toJSON FieldId {fieldId} = Number $ scientific (fromIntegral fieldId) 0