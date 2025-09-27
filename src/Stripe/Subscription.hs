{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Subscription where

import Data.Aeson
import Data.Aeson.Helpers
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import Relude
import Servant
import Stripe.Auth (StripeAuthHeader)
import Stripe.Checkout (LineItem (price, quantity))
import Stripe.Customer (StripeCustomerID)
import Web.FormUrlEncoded

-- | The status of a Stripe Subscription.
--
-- <https://docs.stripe.com/api/subscriptions/object#subscription_object-status>
data SubscriptionStatus
  = -- | the initial payment attempt failed
    SubscriptionStatusIncomplete
  | -- | the first invoice was not paid within 23 hours
    SubscriptionStatusIncompleteExpired
  | SubscriptionStatusTrialing
  | -- | The first invoice was paid
    SubscriptionStatusActive
  | -- | If subscription collection_method=charge_automatically, it becomes past_due when payment is required but cannot be paid (due to failed payment or awaiting additional user actions).
    SubscriptionStatusPastDue
  | -- | if subscription collection_method=send_invoice it becomes past_due when its invoice is not paid by the due date, and canceled or unpaid if it is still not paid by an additional deadline after th
    SubscriptionStatusCanceled
  | SubscriptionStatusUnpaid
  | -- \| A trial ended without a payment method
    SubscriptionStatusPaused
  deriving (Generic, Show, Eq)

instance Validity SubscriptionStatus

instance GenValid SubscriptionStatus

instance FromJSON SubscriptionStatus where
  parseJSON = withText "SubscriptionStatus" $ \case
    "incomplete" -> pure SubscriptionStatusIncomplete
    "incomplete_expired" -> pure SubscriptionStatusIncompleteExpired
    "trialing" -> pure SubscriptionStatusTrialing
    "active" -> pure SubscriptionStatusActive
    "past_due" -> pure SubscriptionStatusPastDue
    "canceled" -> pure SubscriptionStatusCanceled
    "unpaid" -> pure SubscriptionStatusUnpaid
    "paused" -> pure SubscriptionStatusPaused
    s -> fail $ toString $ "Cannot parse SubscriptionStatus JSON: " <> s

instance ToHttpApiData SubscriptionStatus where
  toQueryParam = \case
    SubscriptionStatusIncomplete -> "incomplete"
    SubscriptionStatusIncompleteExpired -> "incomplete_expired"
    SubscriptionStatusTrialing -> "trialing"
    SubscriptionStatusActive -> "active"
    SubscriptionStatusPastDue -> "past_due"
    SubscriptionStatusCanceled -> "canceled"
    SubscriptionStatusUnpaid -> "unpaid"
    SubscriptionStatusPaused -> "paused"

instance ToJSON SubscriptionStatus where
  toJSON = \case
    SubscriptionStatusIncomplete -> toJSON @Text "incomplete"
    SubscriptionStatusIncompleteExpired -> toJSON @Text "incomplete_expired"
    SubscriptionStatusTrialing -> toJSON @Text "trialing"
    SubscriptionStatusActive -> toJSON @Text "active"
    SubscriptionStatusPastDue -> toJSON @Text "past_due"
    SubscriptionStatusCanceled -> toJSON @Text "canceled"
    SubscriptionStatusUnpaid -> toJSON @Text "unpaid"
    SubscriptionStatusPaused -> toJSON @Text "paused"

instance PG.FromField SubscriptionStatus where
  fromField f mBS =
    case decodeUtf8With lenientDecode <$> mBS of
      Just "SubscriptionStatusIncomplete" -> pure SubscriptionStatusIncomplete
      Just "SubscriptionStatusIncompleteExpired" -> pure SubscriptionStatusIncompleteExpired
      Just "SubscriptionStatusTrialing" -> pure SubscriptionStatusTrialing
      Just "SubscriptionStatusActive" -> pure SubscriptionStatusActive
      Just "SubscriptionStatusPastDue" -> pure SubscriptionStatusPastDue
      Just "SubscriptionStatusCanceled" -> pure SubscriptionStatusCanceled
      Just "SubscriptionStatusUnpaid" -> pure SubscriptionStatusUnpaid
      Just "SubscriptionStatusPaused" -> pure SubscriptionStatusPaused
      Just txt -> PG.returnError PG.ConversionFailed f $ toString txt
      Nothing -> PG.returnError PG.UnexpectedNull f ""

instance PG.ToField SubscriptionStatus where
  toField = \case
    SubscriptionStatusIncomplete -> PG.toField @Text "SubscriptionStatusIncomplete"
    SubscriptionStatusIncompleteExpired -> PG.toField @Text "SubscriptionStatusIncompleteExpired"
    SubscriptionStatusTrialing -> PG.toField @Text "SubscriptionStatusTrialing"
    SubscriptionStatusActive -> PG.toField @Text "SubscriptionStatusActive"
    SubscriptionStatusPastDue -> PG.toField @Text "SubscriptionStatusPastDue"
    SubscriptionStatusCanceled -> PG.toField @Text "SubscriptionStatusCanceled"
    SubscriptionStatusUnpaid -> PG.toField @Text "SubscriptionStatusUnpaid"
    SubscriptionStatusPaused -> PG.toField @Text "SubscriptionStatusPaused"

instance SQL.FromField SubscriptionStatus where
  fromField field =
    SQL.fromField field >>= \case
      "SubscriptionStatusIncomplete" -> SQL.Ok SubscriptionStatusIncomplete
      "SubscriptionStatusIncompleteExpired" -> SQL.Ok SubscriptionStatusIncompleteExpired
      "SubscriptionStatusTrialing" -> SQL.Ok SubscriptionStatusTrialing
      "SubscriptionStatusActive" -> SQL.Ok SubscriptionStatusActive
      "SubscriptionStatusPastDue" -> SQL.Ok SubscriptionStatusPastDue
      "SubscriptionStatusCanceled" -> SQL.Ok SubscriptionStatusCanceled
      "SubscriptionStatusUnpaid" -> SQL.Ok SubscriptionStatusUnpaid
      "SubscriptionStatusPaused" -> SQL.Ok SubscriptionStatusPaused
      txt -> fail $ "Could not parse SubscriptionStatus " <> txt

instance SQL.ToField SubscriptionStatus where
  toField = \case
    SubscriptionStatusIncomplete -> SQL.toField @Text "SubscriptionStatusIncomplete"
    SubscriptionStatusIncompleteExpired -> SQL.toField @Text "SubscriptionStatusIncompleteExpired"
    SubscriptionStatusTrialing -> SQL.toField @Text "SubscriptionStatusTrialing"
    SubscriptionStatusActive -> SQL.toField @Text "SubscriptionStatusActive"
    SubscriptionStatusPastDue -> SQL.toField @Text "SubscriptionStatusPastDue"
    SubscriptionStatusCanceled -> SQL.toField @Text "SubscriptionStatusCanceled"
    SubscriptionStatusUnpaid -> SQL.toField @Text "SubscriptionStatusUnpaid"
    SubscriptionStatusPaused -> SQL.toField @Text "SubscriptionStatusPaused"

-- | The ID of a Stripe Subscription.
--
-- <https://docs.stripe.com/api/subscriptions/object#subscription_object-id>
newtype StripeSubscriptionID = StripeSubscriptionID
  {unStripeSubscriptionId :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (PG.ToField, PG.FromField)
  deriving newtype (SQL.ToField, SQL.FromField)
  deriving newtype (ToHttpApiData)

instance GenValid StripeSubscriptionID

instance Validity StripeSubscriptionID

-- | Parameters for creating a Subscription.
--
-- <https://docs.stripe.com/api/subscriptions/create>
data CreateSubscription = CreateSubscription
  { createSubscriptionCustomer :: StripeCustomerID,
    createSubscriptionItems :: LineItem
  }
  deriving (Generic)

instance ToJSON CreateSubscription where
  toJSON = genericToJSON $ customOptionsSnake "CreateSubscription"

instance ToForm CreateSubscription where
  toForm CreateSubscription {..} =
    [ ("customer", toQueryParam createSubscriptionCustomer),
      ("items[0][price]", toQueryParam $ price createSubscriptionItems),
      ("items[0][quantity]", toQueryParam $ quantity createSubscriptionItems)
    ]

-- | A wrapper around UTCTime to handle Stripe's epoch timestamps.
newtype DateTime = DateTime {unDateTime :: UTCTime}
  deriving newtype (Validity, GenValid, PG.ToField, PG.FromField)
  deriving (Generic, Show, Eq)

instance FromJSON DateTime where
  parseJSON = fmap (DateTime . posixSecondsToUTCTime . fromIntegral) . parseJSON @Int

instance ToJSON DateTime where
  toJSON = toJSON @Int . (round . utcTimeToPOSIXSeconds . unDateTime)

-- | The Stripe Subscription object.
--
-- <https://docs.stripe.com/api/subscriptions/object>
data StripeSubscription = StripeSubscription
  { stripeSubscriptionId :: StripeSubscriptionID,
    stripeSubscriptionCustomer :: StripeCustomerID,
    stripeSubscriptionStatus :: SubscriptionStatus
  }
  deriving (Generic, Show, Eq)

instance GenValid StripeSubscription

instance Validity StripeSubscription

instance FromJSON StripeSubscription where
  parseJSON = genericParseJSON $ customOptionsSnake "StripeSubscription"

instance ToJSON StripeSubscription where
  toJSON = genericToJSON $ customOptionsSnake "StripeSubscription"

type CreateStripeSubscriptionAPI =
  "v1"
    :> "subscriptions"
    :> StripeAuthHeader
    :> ReqBody '[FormUrlEncoded] CreateSubscription
    :> Post '[JSON] StripeSubscription

type DeleteStripeSubscriptionAPI =
  "v1"
    :> "subscriptions"
    :> StripeAuthHeader
    :> Capture "subscriptionId" StripeSubscriptionID
    :> Delete '[JSON] StripeSubscription

type StripeSubscriptionsAPI =
  CreateStripeSubscriptionAPI :<|> DeleteStripeSubscriptionAPI
