{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Checkout where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Helpers
import Data.GenValidity
import Data.GenValidity.CustomURI ()
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import GHC.Generics
import Relude
import Servant.API
import Stripe.Auth
import Stripe.Customer
import Stripe.PaymentIntent (PaymentIntentID)
import Web.FormUrlEncoded

-- | The mode of the Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/create#create_checkout_session-mode>
data CheckoutMode = CheckoutPayment | CheckoutSubscription | CheckoutSetup
  deriving (Show, Generic)

instance FromHttpApiData CheckoutMode where
  parseQueryParam "payment" = pure CheckoutPayment
  parseQueryParam "subscription" = pure CheckoutSubscription
  parseQueryParam "setup" = pure CheckoutSetup
  parseQueryParam t = fail $ "Unknown CheckoutMode" <> show t

instance ToHttpApiData CheckoutMode where
  toQueryParam CheckoutPayment = "payment"
  toQueryParam CheckoutSubscription = "subscription"
  toQueryParam CheckoutSetup = "setup"

instance ToJSON CheckoutMode where
  toJSON CheckoutPayment = String "payment"
  toJSON CheckoutSubscription = String "subscription"
  toJSON CheckoutSetup = String "setup"

-- | The ID of a Stripe Price object.
--
-- <https://docs.stripe.com/api/prices/object#price_object-id>
newtype PriceID = PriceID {unPriceID :: Text}
  deriving (Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

-- | A line item for a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/create#create_checkout_session-line_items>
data LineItem = LineItem
  { price :: PriceID,
    quantity :: Int
  }
  deriving (Show, Generic)

instance ToForm LineItem

instance ToJSON LineItem

-- | The ID of a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/object#checkout_session_object-id>
newtype CheckoutSessionID = CheckoutSessionID {unCheckoutSessionId :: Text}
  deriving (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (PG.ToField, PG.FromField)
  deriving newtype (SQL.ToField, SQL.FromField)

instance GenValid CheckoutSessionID

instance Validity CheckoutSessionID

-- | Parameters for creating a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/create>
data CreateCheckoutSession = CreateCheckoutSession
  { successUrl :: Text,
    cancelUrl :: Text,
    mode :: CheckoutMode,
    lineItems :: [LineItem],
    -- | The ID of an existing Customer to use for this Session.
    -- If none is provided, a new Customer will be created.
    customerId :: Maybe StripeCustomerID,
    -- | The ID of the subscription to update.
    subscription :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON CreateCheckoutSession where
  toJSON = genericToJSON (customOptionsSnake "") {omitNothingFields = True}

lineItemsToForm :: [LineItem] -> [(Text, Text)]
lineItemsToForm items =
  concat
    $ zipWith
      ( \i LineItem {..} ->
          [ ("line_items[" <> show i <> "][price]", toQueryParam price),
            ("line_items[" <> show i <> "][quantity]", toQueryParam quantity)
          ]
      )
      [0 :: Int ..]
      items

instance ToForm CreateCheckoutSession where
  toForm CreateCheckoutSession {..} =
    [ ("success_url", successUrl),
      ("cancel_url", cancelUrl),
      ("mode", toQueryParam mode)
    ]
      <> maybe [] (\cid -> [("customer", toQueryParam cid)]) customerId
      <> maybe [] (\sub -> [("subscription", sub)]) subscription
      -- Tricky
      <> case mode of
        CheckoutSetup -> [("currency", "USD")]
        _ -> toForm $ lineItemsToForm lineItems

-- | The Checkout Session object.
--
-- <https://docs.stripe.com/api/checkout/sessions/object>
data CheckoutSession = CheckoutSession
  { checkoutSessionId :: CheckoutSessionID,
    checkoutSessionCustomer :: StripeCustomerID,
    checkoutSessionPaymentStatus :: PaymentStatus,
    checkoutSessionUrl :: Maybe URI,
    checkoutSessionPaymentIntent :: Maybe PaymentIntentID
  }
  deriving (Show, Generic, Eq)

instance FromJSON CheckoutSession where
  parseJSON = genericParseJSON $ customOptionsSnake "CheckoutSession"

instance ToJSON CheckoutSession where
  toJSON = genericToJSON $ customOptionsSnake "CheckoutSession"

instance GenValid CheckoutSession

instance Validity CheckoutSession

-- | The payment status of a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/object#checkout_session_object-payment_status>
data PaymentStatus = Paid | Unpaid | NoPaymentRequired
  deriving (Generic, Show, Eq)

instance FromJSON PaymentStatus where
  parseJSON = withText "PaymentStatus" $ \case
    "paid" -> pure Paid
    "unpaid" -> pure Unpaid
    "no_payment_required" -> pure NoPaymentRequired
    s -> fail $ "Cannot parse PaymentStatus " <> toString s

instance ToJSON PaymentStatus where
  toJSON = toJSON . snakeCase . show

instance GenValid PaymentStatus

instance Validity PaymentStatus

--------------------------------------------------------------------------------

type StripeCheckoutAPI =
  "v1"
    :> "checkout"
    :> "sessions"
    :> Header' '[Required] "Authorization" StripeAPIKey
    :> ReqBody '[FormUrlEncoded] CreateCheckoutSession
    :> Post '[JSON] CheckoutSession
