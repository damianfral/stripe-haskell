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

data StripeEventObject
  = CheckoutSessionCompleted CheckoutSession
  | CustomerSubscriptionCreated StripeSubscription
  | CustomerSubscriptionUpdated StripeSubscription
  | CustomerSubscriptionDeleted StripeSubscription
  | InvoicePaid StripeInvoice
  | InvoicePaymentFailed StripeInvoice
  | Other Value
  deriving (Generic, Show, Eq)

instance ToJSON StripeEventObject where
  toJSON (CheckoutSessionCompleted o) = toJSON o
  toJSON (CustomerSubscriptionCreated o) = toJSON o
  toJSON (CustomerSubscriptionUpdated o) = toJSON o
  toJSON (CustomerSubscriptionDeleted o) = toJSON o
  toJSON (InvoicePaid o) = toJSON o
  toJSON (InvoicePaymentFailed o) = toJSON o
  toJSON (Other o) = o

instance Validity StripeEventObject

instance GenValid StripeEventObject where
  genValid =
    oneof
      [ CheckoutSessionCompleted <$> genValid,
        CustomerSubscriptionCreated <$> genValid,
        CustomerSubscriptionUpdated <$> genValid,
        CustomerSubscriptionDeleted <$> genValid,
        InvoicePaid <$> genValid,
        InvoicePaymentFailed <$> genValid,
        Other <$> genValid
      ]
