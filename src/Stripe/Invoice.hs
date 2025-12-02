{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Invoice where

import Data.Aeson
import Data.Aeson.Helpers
import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Data.Text as T
import Relude
import Stripe.Customer (StripeCustomerID)
import Stripe.Subscription (StripeSubscriptionID)

newtype StripeInvoiceID = StripeInvoiceID {unStripeInvoiceID :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

instance Validity StripeInvoiceID

instance GenValid StripeInvoiceID where
  genValid = StripeInvoiceID . ("in_" <>) <$> genValid
  shrinkValid (StripeInvoiceID t) =
    [StripeInvoiceID s | s <- shrinkValid t, "in_" `T.isPrefixOf` s]

data InvoiceStatus
  = InvoiceStatusDraft
  | InvoiceStatusOpen
  | InvoiceStatusPaid
  | InvoiceStatusUncollectible
  | InvoiceStatusVoid
  deriving (Generic, Show, Eq)

instance Validity InvoiceStatus

instance GenValid InvoiceStatus

instance FromJSON InvoiceStatus where
  parseJSON = withText "InvoiceStatus" $ \case
    "draft" -> pure InvoiceStatusDraft
    "open" -> pure InvoiceStatusOpen
    "paid" -> pure InvoiceStatusPaid
    "uncollectible" -> pure InvoiceStatusUncollectible
    "void" -> pure InvoiceStatusVoid
    s -> fail $ toString $ "Cannot parse InvoiceStatus JSON: " <> s

instance ToJSON InvoiceStatus where
  toJSON = \case
    InvoiceStatusDraft -> toJSON @Text "draft"
    InvoiceStatusOpen -> toJSON @Text "open"
    InvoiceStatusPaid -> toJSON @Text "paid"
    InvoiceStatusUncollectible -> toJSON @Text "uncollectible"
    InvoiceStatusVoid -> toJSON @Text "void"

data StripeInvoice = StripeInvoice
  { stripeInvoiceId :: StripeInvoiceID,
    stripeInvoiceCustomer :: StripeCustomerID,
    stripeInvoiceSubscription :: Maybe StripeSubscriptionID,
    stripeInvoiceStatus :: InvoiceStatus,
    stripeInvoiceHostedInvoiceUrl :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance Validity StripeInvoice

instance GenValid StripeInvoice

instance FromJSON StripeInvoice where
  parseJSON = genericParseJSON $ customOptionsSnake "StripeInvoice"

instance ToJSON StripeInvoice where
  toJSON = genericToJSON $ customOptionsSnake "StripeInvoice"
