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
import GHC.Generics
import Relude
import Servant.API
import Stripe.Auth
import Stripe.Customer
import Web.FormUrlEncoded

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

newtype PriceID = PriceID {unPriceID :: Text}
  deriving (Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData)

data LineItem = LineItem
  { price :: PriceID,
    quantity :: Int
  }
  deriving (Show, Generic)

instance ToForm LineItem

instance ToJSON LineItem

newtype CheckoutSessionID = CheckoutSessionID {unCheckoutSessionId :: Text}
  deriving (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON)

instance GenValid CheckoutSessionID

instance Validity CheckoutSessionID

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

data CheckoutSession = CheckoutSession
  { checkoutSessionId :: CheckoutSessionID,
    checkoutSessionCustomer :: StripeCustomerID,
    checkoutSessionPaymentStatus :: PaymentStatus,
    checkoutSessionUrl :: Maybe Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON CheckoutSession where
  parseJSON = genericParseJSON $ customOptionsSnake "CheckoutSession"

instance ToJSON CheckoutSession where
  toJSON = genericToJSON $ customOptionsSnake "CheckoutSession"

instance GenValid CheckoutSession

instance Validity CheckoutSession

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
