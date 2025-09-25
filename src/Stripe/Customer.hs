{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Customer where

import Data.Aeson
import Data.Aeson.Helpers
import Data.EmailAddress (EmailAddress (unEmailAddress))
import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import Relude
import Servant
import Stripe.Auth (StripeAuthHeader)
import Web.FormUrlEncoded

newtype StripeCustomerID = StripeCustomerID {unStripeCustomerID :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (PG.ToField, PG.FromField)
  deriving newtype (SQL.ToField, SQL.FromField)
  deriving newtype (ToHttpApiData)

instance GenValid StripeCustomerID

instance Validity StripeCustomerID

newtype CreateCustomer = CreateCustomer {createCustomerEmail :: EmailAddress}
  deriving (Generic, Show)

instance ToJSON CreateCustomer where
  toJSON = genericToJSON $ customOptionsSnake "CreateCustomer"

instance ToForm CreateCustomer where
  toForm CreateCustomer {..} =
    [("email", unEmailAddress createCustomerEmail)]

newtype StripeCustomer = StripeCustomer {stripeCustomerId :: StripeCustomerID}
  deriving (Generic, Show, Eq)

instance GenValid StripeCustomer

instance Validity StripeCustomer

instance FromJSON StripeCustomer where
  parseJSON = genericParseJSON $ customOptionsSnake "StripeCustomer"

instance ToJSON StripeCustomer where
  toJSON = genericToJSON $ customOptionsSnake "StripeCustomer"

type StripeCustomersAPI =
  "v1"
    :> "customers"
    :> StripeAuthHeader
    :> ReqBody '[FormUrlEncoded] CreateCustomer
    :> Post '[JSON] StripeCustomer
