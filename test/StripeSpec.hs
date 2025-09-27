{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module StripeSpec (spec) where

import qualified Data.Aeson as JSON
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Relude
import Stripe.Checkout
import Stripe.Customer
import Stripe.Event
import Stripe.Event.Object
import Stripe.Invoice
import Stripe.Price as Price
import Stripe.Product as Product
import Stripe.Subscription
import Stripe.Webhook
import Test.Syd
import Test.Syd.Validity (forAllValid)
import Test.Syd.Validity.Aeson (jsonSpec)

spec :: Spec
spec = do
  describe "Stripe Webhook" $ do
    it "validates a correct signature" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      now <- liftIO getCurrentTime
      let timestamp = show @Text @Int $ round $ utcTimeToPOSIXSeconds now
          signature = computeSignature secret now body
          header = "t=" <> timestamp <> ",v1=" <> signature
      liftIO (isValidSignature secret body header) `shouldReturn` True

    it "rejects an incorrect signature" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      now <- liftIO getPOSIXTime
      let timestamp = show @Text @Int $ round now
          signature = "incorrect_signature"
          header = "t=" <> timestamp <> ",v1=" <> signature
      liftIO (isValidSignature secret body header) `shouldReturn` False

    it "rejects a tampered request body" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      now <- liftIO getCurrentTime
      let timestamp = show @Text @Int $ round $ utcTimeToPOSIXSeconds now
          signature = computeSignature secret now body
          header = "t=" <> timestamp <> ",v1=" <> signature
          tamperedBody = "{\"id\":\"evt_tampered\",\"object\":\"event\"}"
      liftIO (isValidSignature secret tamperedBody header) `shouldReturn` False

    it "rejects an expired timestamp" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      -- 5 minutes and 1 second ago
      let expiredTimestamp = "1000000000"
          signature = computeSignature secret (posixSecondsToUTCTime 1000000000) body
          header = "t=" <> expiredTimestamp <> ",v1=" <> signature
      liftIO (isValidSignature secret body header) `shouldReturn` False

    it "rejects a timestamp from the future" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      now <- liftIO getPOSIXTime
      -- 5 minutes and 1 second in the future
      let futureTimestamp = show @Text @Int (round now + 301)
          signature = computeSignature secret (posixSecondsToUTCTime $ now + 301) body
          header = "t=" <> futureTimestamp <> ",v1=" <> signature
      liftIO (isValidSignature secret body header) `shouldReturn` False

    it "rejects a header without a timestamp" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      now <- liftIO getCurrentTime
      let signature = computeSignature secret now body
          header = "v1=" <> signature
      liftIO (isValidSignature secret body header) `shouldReturn` False

    it "rejects a header without a signature" $ forAllValid $ \(secret, obj) -> do
      let body = toStrict $ JSON.encode @JSON.Value obj
      now <- liftIO getPOSIXTime
      let timestamp = show @Text @Int $ round now
          header = "t=" <> timestamp
      liftIO (isValidSignature secret body header) `shouldReturn` False

  describe "Stripe Subscription" $ do
    jsonSpec @StripeSubscription

    it "can decode the sample JSON object" $ do
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

    it "can decode the sample JSON object" $ do
      let filepath = "test-resources/stripe/customer.json"
      let expected = StripeCustomer $ StripeCustomerID "cus_NffrFeUfNV2Hib"
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "Stripe Checkout Session" $ do
    jsonSpec @CheckoutSession

    it "can decode the sample JSON object" $ do
      let filepath = "test-resources/stripe/checkout-session.json"
      let expected =
            CheckoutSession
              { checkoutSessionId = CheckoutSessionID "cs_test_a11YYufWQzNY63zpQ6QSNRQhkUpVph4WRmzW0zWJO2znZKdVujZ0N0S22u",
                checkoutSessionCustomer = StripeCustomerID "cus_Na6dX7aXxi11N4",
                checkoutSessionPaymentStatus = Unpaid,
                checkoutSessionUrl = Just "https://checkout.stripe.com/c/pay/cs_test_a11YYufWQzNY63zpQ6QSNRQhkUpVph4WRmzW0zWJO2znZKdVujZ0N0S22u#fidkdWxOYHwnPyd1blpxYHZxWjA0SDdPUW5JbmFMck1wMmx9N2BLZjFEfGRUNWhqTmJ%2FM2F8bUA2SDRySkFdUV81T1BSV0YxcWJcTUJcYW5rSzN3dzBLPUE0TzRKTTxzNFBjPWZEX1NKSkxpNTVjRjN8VHE0YicpJ2N3amhWYHdzYHcnP3F3cGApJ2lkfGpwcVF8dWAnPyd2bGtiaWBabHFgaCcpJ2BrZGdpYFVpZGZgbWppYWB3dic%2FcXdwYHgl"
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "StripeInvoice" $ do
    jsonSpec @StripeInvoice

  describe "StripeEvent" $ do
    jsonSpec @StripeEvent

    it "can decode the sample subscription created event" $ do
      let filepath = "test-resources/stripe/subscription-event.json"
      let expected =
            StripeEvent
              { stripeEventId = "evt_1NG8Du2eZvKYlo2CUI79vXWy",
                stripeEventObject =
                  CustomerSubscriptionCreated
                    StripeSubscription
                      { stripeSubscriptionCustomer = StripeCustomerID "cus_Na6dX7aXxi11N4",
                        stripeSubscriptionId = StripeSubscriptionID "sub_1MowQVLkdIwHu7ixeRlqHVzs",
                        stripeSubscriptionStatus = SubscriptionStatusActive
                      }
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

    it "can decode the sample invoice paid event" $ do
      let filepath = "test-resources/stripe/invoice-paid-event.json"
      let expected =
            StripeEvent
              { stripeEventId = "evt_invoice_paid_123",
                stripeEventObject =
                  InvoicePaid
                    StripeInvoice
                      { stripeInvoiceId = StripeInvoiceID "in_1IieGZLkdIwHu7ixOqXGqL4i",
                        stripeInvoiceCustomer = StripeCustomerID "cus_NeZwdNtLEOXuvB",
                        stripeInvoiceSubscription = Just (StripeSubscriptionID "sub_1MowQVLkdIwHu7ixeRlqHVzs"),
                        stripeInvoiceStatus = InvoiceStatusPaid,
                        stripeInvoiceHostedInvoiceUrl = Just "https://invoice.stripe.com/i/acct_123/invst_ABC"
                      }
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

    it "can decode the sample invoice payment failed event" $ do
      let filepath = "test-resources/stripe/invoice-payment-failed-event.json"
      let expected =
            StripeEvent
              { stripeEventId = "evt_invoice_payment_failed_123",
                stripeEventObject =
                  InvoicePaymentFailed
                    StripeInvoice
                      { stripeInvoiceId = StripeInvoiceID "in_1IieGZLkdIwHu7ixOqXGqL4j",
                        stripeInvoiceCustomer = StripeCustomerID "cus_NeZwdNtLEOXuvB",
                        stripeInvoiceSubscription = Just (StripeSubscriptionID "sub_1MowQVLkdIwHu7ixeRlqHVzs"),
                        stripeInvoiceStatus = InvoiceStatusOpen,
                        stripeInvoiceHostedInvoiceUrl = Just "https://invoice.stripe.com/i/acct_123/invst_DEF"
                      }
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "StripeProduct" $ do
    jsonSpec @StripeProduct

    it "can decode the sample JSON object" $ do
      let filepath = "test-resources/stripe/product.json"
      let expected =
            StripeProduct
              { stripeProductId = StripeProductID "prod_NWjs8kKbJWmuuc",
                stripeProductName = "Gold Plan",
                stripeProductActive = True,
                stripeProductCreated = 1678833149,
                stripeProductUpdated = 1678833149,
                stripeProductDescription = Nothing,
                stripeProductDefaultPrice = Nothing,
                stripeProductImages = [],
                stripeProductLivemode = False,
                stripeProductMetadata = mempty,
                stripeProductPackageDimensions = Nothing,
                stripeProductShippable = Nothing,
                stripeProductStatementDescriptor = Nothing,
                stripeProductTaxCode = Nothing,
                stripeProductUnitLabel = Nothing,
                stripeProductUrl = Nothing
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)

  describe "StripePrice" $ do
    jsonSpec @StripePrice

    it "can decode the sample JSON object" $ do
      let filepath = "test-resources/stripe/price.json"
      let expected =
            StripePrice
              { stripePriceId = StripePriceID "price_1MoBy5LkdIwHu7ixZhnattbh",
                stripePriceActive = True,
                stripePriceCurrency = "usd",
                stripePriceMetadata = mempty,
                stripePriceNickname = Nothing,
                stripePriceProduct = StripeProductID "prod_NZKdYqrwEYx6iK",
                stripePriceRecurring =
                  Just
                    $ fromList
                      [ ("aggregate_usage", JSON.Null),
                        ("interval", "month"),
                        ("interval_count", JSON.Number 1.0),
                        ("trial_period_days", JSON.Null),
                        ("usage_type", "licensed")
                      ],
                stripePriceTaxBehavior = Just "unspecified",
                stripePriceObject = "price",
                stripePriceBillingScheme = "per_unit",
                stripePriceCreated = 1679431181,
                stripePriceLivemode = False,
                stripePriceLookupKey = Nothing,
                stripePriceTiersMode = Nothing,
                stripePriceTransformQuantity = Nothing,
                stripePricePriceType = "recurring",
                stripePriceUnitAmount = Just 1000,
                stripePriceUnitAmountDecimal = Just "1000"
              }
      JSON.eitherDecodeFileStrict filepath >>= flip shouldBe (pure expected)
