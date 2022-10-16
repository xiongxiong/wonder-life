{-# LANGUAGE DeriveGeneric #-}
module Api.Basic
(   ApiResult(..),
    FieldId(..)
) where

import RIO (Int, Text, Int64, Eq, Show, Generic, ($), fromIntegral, (<$>))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (..), withObject, (.:))
import Data.Aeson.KeyMap (fromList)
import Data.Scientific (scientific)

data ApiResult a = ApiResultSuccess a | ApiResultFailure Int Text

instance ToJSON a => ToJSON (ApiResult a) where
  toJSON (ApiResultSuccess value) = Object $ fromList [("code", Number 0), ("data", toJSON value)]
  toJSON (ApiResultFailure code message) = Object $ fromList [("code", Number $ scientific (fromIntegral code) 0), ("message", String message)]

newtype FieldId = FieldId {fieldId :: Int64} deriving (Eq, Show, Generic)

instance FromJSON FieldId where
  parseJSON = withObject "FieldId" $ \v -> FieldId <$> v .: "id"