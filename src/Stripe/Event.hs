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

-- | Events are our way of letting you know when something interesting happens in
-- | your account. When an interesting event occurs, we create a new `Event`
-- | object. For example, when a charge succeeds, we create a `charge.succeeded`
-- | event; and when an invoice payment attempt fails, we create an
-- | `invoice.payment_failed` event. Note that many API requests may cause
-- | multiple events to be created. For example, if you create a new subscription
-- | for a customer, you will see both a `customer.subscription.created` event
-- | and a `charge.succeeded` event.
-- |
-- | <https://docs.stripe.com/api/events/object>
data StripeEvent = StripeEvent
  { -- | Unique identifier for the object.
    stripeEventId :: Text,
    -- | The object containing the event data.
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
      "payment_intent.created" -> PaymentIntentCreated <$> parseJSON obj
      "payment_intent.succeeded" -> PaymentIntentSucceeded <$> parseJSON obj
      "payment_intent.payment_failed" -> PaymentIntentPaymentFailed <$> parseJSON obj
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
          InvoicePaymentFailed _ -> "invoice.payment_failed"
          PaymentIntentCreated _ -> "payment_intent.created"
          PaymentIntentSucceeded _ -> "payment_intent.succeeded"
          PaymentIntentPaymentFailed _ -> "payment_intent.payment_failed",
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
          InvoicePaymentFailed <$> genValid,
          PaymentIntentCreated <$> genValid,
          PaymentIntentSucceeded <$> genValid,
          PaymentIntentPaymentFailed <$> genValid
        ]
    pure StripeEvent {..}
