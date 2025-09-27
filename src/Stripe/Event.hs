{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Event where

import Data.Aeson
import Data.GenValidity
import Relude
import Stripe.Event.Object
import Test.QuickCheck.Gen (oneof)

data StripeEvent = StripeEvent
  { stripeEventId :: Text,
    stripeEventObject :: StripeEventObject
  }
  deriving (Generic, Show, Eq)

instance FromJSON StripeEvent where
  parseJSON = withObject "StripeEvent" $ \o -> do
    eId <- o .: "id"
    eType <- o .: "type"
    dat <- o .: "data"
    obj <- dat .: "object"
    eObject <- case (eType :: Text) of
      "checkout.session.completed" -> CheckoutSessionCompleted <$> parseJSON obj
      "customer.subscription.created" -> CustomerSubscriptionCreated <$> parseJSON obj
      "customer.subscription.updated" -> CustomerSubscriptionUpdated <$> parseJSON obj
      "customer.subscription.deleted" -> CustomerSubscriptionDeleted <$> parseJSON obj
      "invoice.paid" -> InvoicePaid <$> parseJSON obj
      "invoice.payment_failed" -> InvoicePaymentFailed <$> parseJSON obj
      str -> fail $ "Could not parse StripeEvent.type " <> toString str
    pure $ StripeEvent eId eObject

instance ToJSON StripeEvent where
  toJSON event =
    object
      [ "id" .= stripeEventId event,
        "type" .= case stripeEventObject event of
          CheckoutSessionCompleted _ -> "checkout.session.completed"
          CustomerSubscriptionCreated _ -> "customer.subscription.created"
          CustomerSubscriptionUpdated _ -> "customer.subscription.updated"
          CustomerSubscriptionDeleted _ -> "customer.subscription.deleted"
          InvoicePaid _ -> "invoice.paid" :: Text
          InvoicePaymentFailed _ -> "invoice.payment_failed",
        "data" .= object ["object" .= stripeEventObject event]
      ]

instance Validity StripeEvent

instance GenValid StripeEvent where
  genValid = do
    stripeEventId <- genValid
    stripeEventObject <-
      oneof
        [ CheckoutSessionCompleted <$> genValid,
          CustomerSubscriptionCreated <$> genValid,
          CustomerSubscriptionUpdated <$> genValid,
          CustomerSubscriptionDeleted <$> genValid,
          InvoicePaid <$> genValid,
          InvoicePaymentFailed <$> genValid
        ]
    pure StripeEvent {..}
