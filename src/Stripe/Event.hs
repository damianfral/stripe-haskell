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
    stripeEventType :: Text,
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
      _ -> pure $ Other obj
    pure $ StripeEvent eId eType eObject

instance ToJSON StripeEvent where
  toJSON event =
    object
      [ "id" .= stripeEventId event,
        "type" .= stripeEventType event,
        "data" .= object ["object" .= stripeEventObject event]
      ]

instance Validity StripeEvent

instance GenValid StripeEvent where
  genValid = do
    stripeEventId <- genValid
    (stripeEventType, stripeEventObject) <-
      oneof
        [ do
            o <- genValid
            pure ("checkout.session.completed", CheckoutSessionCompleted o),
          do
            o <- genValid
            pure ("customer.subscription.created", CustomerSubscriptionCreated o),
          do
            o <- genValid
            pure ("customer.subscription.updated", CustomerSubscriptionUpdated o),
          do
            o <- genValid
            pure ("customer.subscription.deleted", CustomerSubscriptionDeleted o),
          do
            o <- genValid
            pure ("invoice.paid", InvoicePaid o),
          do
            o <- genValid
            pure ("invoice.payment_failed", InvoicePaymentFailed o),
          do
            o <- genValid
            -- Any other event type will be parsed as 'Other'
            pure ("some.other.event", Other o)
        ]
    pure StripeEvent {..}
