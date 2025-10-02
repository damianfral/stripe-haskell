{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.EmailAddress where

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Relude
import Servant (ToHttpApiData)

newtype EmailAddress = EmailAddress {unEmailAddress :: Text}
  deriving newtype (Show, PG.ToField, PG.FromField, ToJSON, FromJSON, ToHttpApiData, Eq, IsString)
  deriving (Generic)
  deriving newtype (Validity, GenValid)
