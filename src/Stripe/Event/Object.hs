{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Event.Object where

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Relude
import Stripe.Checkout
import Stripe.Invoice
import Stripe.Subscription
import Test.QuickCheck.Gen (oneof)

-- | This object contains the API resource relevant to the event. For example, an
-- | `invoice.created` event will have a full invoice object as the value of the
-- | object key.
-- |
-- | <https://docs.stripe.com/api/events/object#event_object-data-object>
data StripeEventObject
  = -- | Occurs when a Checkout Session has been successfully completed.
    CheckoutSessionCompleted CheckoutSession
  | -- | Occurs whenever a customer is signed up for a new subscription.
    CustomerSubscriptionCreated StripeSubscription
  | -- | Occurs whenever a subscription changes.
    CustomerSubscriptionUpdated StripeSubscription
  | -- | Occurs whenever a customer's subscription ends.
    CustomerSubscriptionDeleted StripeSubscription
  | -- | Occurs whenever an invoice is paid.
    InvoicePaid StripeInvoice
  | -- | Occurs whenever an invoice payment attempt fails.
    InvoicePaymentFailed StripeInvoice
  deriving (Generic, Show, Eq)

instance ToJSON StripeEventObject where
  toJSON (CheckoutSessionCompleted o) = toJSON o
  toJSON (CustomerSubscriptionCreated o) = toJSON o
  toJSON (CustomerSubscriptionUpdated o) = toJSON o
  toJSON (CustomerSubscriptionDeleted o) = toJSON o
  toJSON (InvoicePaid o) = toJSON o
  toJSON (InvoicePaymentFailed o) = toJSON o

instance Validity StripeEventObject

instance GenValid StripeEventObject where
  genValid =
    oneof
      [ CheckoutSessionCompleted <$> genValid,
        CustomerSubscriptionCreated <$> genValid,
        CustomerSubscriptionUpdated <$> genValid,
        CustomerSubscriptionDeleted <$> genValid,
        InvoicePaid <$> genValid,
        InvoicePaymentFailed <$> genValid
      ]
