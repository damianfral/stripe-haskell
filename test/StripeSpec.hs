{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StripeSpec (spec) where

import qualified Data.Aeson as JSON
import Relude
import Stripe.Checkout
import Stripe.Customer
import Stripe.Event
import Stripe.Subscription
import Test.Syd
import Test.Syd.Validity.Aeson (jsonSpec)

spec :: Spec
spec = do
  describe "Stripe Subscription" $ do
    jsonSpec @StripeSubscription

    it "can decode a JSON object" $ do
      let filepath = "test-resources/stripe/subscription.json"
      let expected =
            StripeSubscription
              { stripeSubscriptionCustomer = StripeCustomerID "cus_QrL3lZXRKcqknt",
                stripeSubscriptionId = StripeSubscriptionID "sub_1PzcNVBBAD3j0rJ3ZA2Z3ip7",
                stripeSubscriptionStatus = SubscriptionStatusTrialing
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "Stripe Customer" $ do
    jsonSpec @StripeCustomer

    it "can decode a JSON object" $ do
      let filepath = "test-resources/stripe/customer.json"
      let expected = StripeCustomer $ StripeCustomerID "cus_NffrFeUfNV2Hib"
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "Stripe Checkout Session" $ do
    jsonSpec @CheckoutSession

    it "can decode a JSON object" $ do
      let filepath = "test-resources/stripe/checkout-session.json"
      let expected =
            CheckoutSession
              { checkoutSessionId = CheckoutSessionID "cs_test_a11YYufWQzNY63zpQ6QSNRQhkUpVph4WRmzW0zWJO2znZKdVujZ0N0S22u",
                checkoutSessionCustomer = StripeCustomerID "cus_Na6dX7aXxi11N4",
                checkoutSessionPaymentStatus = Unpaid,
                checkoutSessionUrl = Just "https://checkout.stripe.com/c/pay/cs_test_a11YYufWQzNY63zpQ6QSNRQhkUpVph4WRmzW0zWJO2znZKdVujZ0N0S22u#fidkdWxOYHwnPyd1blpxYHZxWjA0SDdPUW5JbmFMck1wMmx9N2BLZjFEfGRUNWhqTmJ%2FM2F8bUA2SDRySkFdUV81T1BSV0YxcWJcTUJcYW5rSzN3dzBLPUE0TzRKTTxzNFBjPWZEX1NKSkxpNTVjRjN8VHE0YicpJ2N3amhWYHdzYHcnP3F3cGApJ2lkfGpwcVF8dWAnPyd2bGtiaWBabHFgaCcpJ2BrZGdpYFVpZGZgbWppYWB3dic%2FcXdwYHgl"
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "StripeEvent" $ do
    it "can decode a JSON event" $ do
      let filepath = "test-resources/stripe/subscription-event.json"
      let expected =
            StripeEvent
              { stripeEventId = "evt_1NG8Du2eZvKYlo2CUI79vXWy",
                stripeEventType = "customer.subscription.created",
                stripeEventObject =
                  StripeSubscription
                    { stripeSubscriptionCustomer = StripeCustomerID "cus_Na6dX7aXxi11N4",
                      stripeSubscriptionId = StripeSubscriptionID "sub_1MowQVLkdIwHu7ixeRlqHVzs",
                      stripeSubscriptionStatus = SubscriptionStatusActive
                    }
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)
