{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.PaymentIntent where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Helpers
import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import GHC.Generics
import Relude
import Stripe.Customer (StripeCustomerID)

-- | The ID of a Payment Intent.
--
-- <https://docs.stripe.com/api/payment_intents/object#payment_intent_object-id>
newtype PaymentIntentID = PaymentIntentID {unPaymentIntentID :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (PG.ToField, PG.FromField)
  deriving newtype (SQL.ToField, SQL.FromField)

instance Validity PaymentIntentID

instance GenValid PaymentIntentID where
  genValid = PaymentIntentID . ("pi_" <>) <$> genValid
  shrinkValid (PaymentIntentID t) =
    [PaymentIntentID s | s <- shrinkValid t, "pi_" `T.isPrefixOf` s]

-- | The status of a Payment Intent.
--
-- <https://docs.stripe.com/api/payment_intents/object#payment_intent_object-status>
data PaymentIntentStatus
  = Canceled
  | Processing
  | RequiresAction
  | RequiresCapture
  | RequiresConfirmation
  | RequiresPaymentMethod
  | Succeeded
  deriving (Show, Eq, Generic)

instance FromJSON PaymentIntentStatus where
  parseJSON = withText "PaymentIntentStatus" $ \case
    "canceled" -> pure Canceled
    "processing" -> pure Processing
    "requires_action" -> pure RequiresAction
    "requires_capture" -> pure RequiresCapture
    "requires_confirmation" -> pure RequiresConfirmation
    "requires_payment_method" -> pure RequiresPaymentMethod
    "succeeded" -> pure Succeeded
    s -> fail $ "Cannot parse PaymentIntentStatus: " <> toString s

instance ToJSON PaymentIntentStatus where
  toJSON = toJSON . snakeCase . show

instance SQL.FromField PaymentIntentStatus where
  fromField =
    SQL.fromField @Text >=> \case
      "canceled" -> pure Canceled
      "processing" -> pure Processing
      "requires_action" -> pure RequiresAction
      "requires_capture" -> pure RequiresCapture
      "requires_confirmation" -> pure RequiresConfirmation
      "requires_payment_method" -> pure RequiresPaymentMethod
      "succeeded" -> pure Succeeded
      s -> fail $ "Cannot parse PaymentIntentStatus: " <> toString s

instance SQL.ToField PaymentIntentStatus where
  toField = SQL.toField . snakeCase . show

instance Validity PaymentIntentStatus

instance GenValid PaymentIntentStatus

-- | A payment error.
--
-- <https://stripe.com/docs/api/errors>
newtype PaymentError = PaymentError {paymentErrorMessage :: Text}
  deriving (Eq, Show, Generic)

instance Validity PaymentError

instance GenValid PaymentError where
  genValid = PaymentError <$> genValid

instance FromJSON PaymentError where
  parseJSON = withObject "PaymentError" $ \o -> PaymentError <$> o .: "message"

instance ToJSON PaymentError where
  toJSON = genericToJSON (customOptionsSnake "PaymentError")

-- | The Payment Intent object.
--
-- <https://docs.stripe.com/api/payment_intents/object>
data PaymentIntent = PaymentIntent
  { paymentIntentId :: PaymentIntentID,
    paymentIntentAmount :: Int,
    paymentIntentCurrency :: Text,
    paymentIntentStatus :: PaymentIntentStatus,
    paymentIntentClientSecret :: Maybe Text,
    paymentIntentLastPaymentError :: Maybe PaymentError,
    paymentIntentCustomer :: Maybe StripeCustomerID
  }
  deriving (Show, Eq, Generic)

instance FromJSON PaymentIntent where
  parseJSON = genericParseJSON (customOptionsSnake "PaymentIntent")

instance ToJSON PaymentIntent where
  toJSON = genericToJSON (customOptionsSnake "PaymentIntent")

instance Validity PaymentIntent

instance GenValid PaymentIntent
