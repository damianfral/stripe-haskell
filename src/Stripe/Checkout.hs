{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Checkout where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Helpers
import Data.ByteString.Lazy (toStrict)
import Data.EmailAddress (EmailAddress)
import Data.GenValidity
import Data.GenValidity.Text ()
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import Database.SQLite.Simple
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.Ok as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import GHC.Generics
import Network.URI.Orphans ()
import Relude hiding (decodeUtf8, encodeUtf8, toStrict)
import Servant.API
import Stripe.Auth
import Stripe.Customer
import Stripe.PaymentIntent (PaymentIntentID)
import Stripe.Price
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

-- | A line item for a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/create#create_checkout_session-line_items>
data LineItem = LineItem
  { price :: StripePriceID,
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

instance GenValid CheckoutSessionID where
  genValid = CheckoutSessionID . ("cs_" <>) <$> genValid
  shrinkValid (CheckoutSessionID t) =
    [CheckoutSessionID s | s <- shrinkValid t, "cs_" `T.isPrefixOf` s]

instance Validity CheckoutSessionID

-- | The ID of a Coupon.
--
-- <https://docs.stripe.com/api/coupons/object#coupon_object-id>
newtype CouponID = CouponID {unCouponID :: Text}
  deriving (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (ToHttpApiData, FromHttpApiData)
  deriving newtype (PG.ToField, PG.FromField)
  deriving newtype (SQL.ToField, SQL.FromField)

instance GenValid CouponID where
  genValid = CouponID . ("co_" <>) <$> genValid
  shrinkValid (CouponID t) =
    [CouponID s | s <- shrinkValid t, "co_" `T.isPrefixOf` s]

instance Validity CouponID

-- | The ID of a Promotion Code.
--
-- <https://docs.stripe.com/api/promotion_codes/object#promotion_code_object-id>
newtype PromotionCodeID = PromotionCodeID {unPromotionCodeID :: Text}
  deriving (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (ToHttpApiData, FromHttpApiData)
  deriving newtype (PG.ToField, PG.FromField)
  deriving newtype (SQL.ToField, SQL.FromField)

instance GenValid PromotionCodeID where
  genValid = PromotionCodeID . ("promo_" <>) <$> genValid
  shrinkValid (PromotionCodeID t) =
    [PromotionCodeID s | s <- shrinkValid t, "promo_" `T.isPrefixOf` s]

instance Validity PromotionCodeID

-- | A discount to apply to a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/create#create_checkout_session-discounts>
data DiscountRequest = DiscountRequest
  { discountCoupon :: Maybe CouponID,
    discountPromotionCode :: Maybe PromotionCodeID
  }
  deriving (Show, Generic)

instance GenValid DiscountRequest

instance Validity DiscountRequest

instance FromJSON DiscountRequest where
  parseJSON = genericParseJSON $ customOptionsSnake "DiscountRequest"

instance ToJSON DiscountRequest where
  toJSON = genericToJSON $ customOptionsSnake "DiscountRequest"

-- | The total details of a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/object#checkout_session_object-total_details>
newtype TotalDetails = TotalDetails {totalDetailsAmountDiscount :: Maybe Int}
  deriving (Show, Generic, Eq)

instance FromJSON TotalDetails where
  parseJSON = genericParseJSON $ customOptionsSnake "TotalDetails"

instance ToJSON TotalDetails where
  toJSON = genericToJSON $ customOptionsSnake "TotalDetails"

instance GenValid TotalDetails

instance Validity TotalDetails

instance SQL.FromField TotalDetails where
  fromField =
    SQL.fromField @Text >=> \t ->
      case eitherDecodeStrict (encodeUtf8 t) of
        Right td -> pure td
        Left e -> fail e

instance SQL.ToField TotalDetails where
  toField = SQL.toField . decodeUtf8 . toStrict . encode

-- | Parameters for creating a Checkout Session.
--
-- <https://docs.stripe.com/api/checkout/sessions/create>
data CreateCheckoutSession = CreateCheckoutSession
  { successUrl :: URI,
    cancelUrl :: URI,
    mode :: CheckoutMode,
    lineItems :: [LineItem],
    -- | The ID of an existing Customer to use for this Session.
    -- If none is provided, a new Customer will be created.
    customerId :: Maybe StripeCustomerID,
    customerEmail :: Maybe EmailAddress,
    -- | The ID of the subscription to update.
    subscription :: Maybe Text,
    -- | If @True@, allow customers to enter a promotion code at checkout.
    allowPromotionCodes :: Maybe Bool,
    -- | Discounts to apply to this Session.
    discounts :: Maybe [DiscountRequest]
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

discountsToForm :: [DiscountRequest] -> [(Text, Text)]
discountsToForm = concat . zipWith ixDiscountToTextPair [0 :: Int ..]
  where
    ixDiscountToTextPair i DiscountRequest {..} =
      catMaybes
        [ discountRequestToCoupon i <$> discountCoupon,
          discountRequestToPromotionCode i <$> discountCoupon
        ]
    discountRequestToCoupon i c =
      ("discounts[" <> show i <> "][coupon]", toQueryParam c)
    discountRequestToPromotionCode i p =
      ("discounts[" <> show i <> "][promotion_code]", toQueryParam p)

instance ToForm CreateCheckoutSession where
  toForm CreateCheckoutSession {..} =
    [ ("success_url", show successUrl),
      ("cancel_url", show cancelUrl),
      ("mode", toQueryParam mode)
    ]
      <> maybe [] (\cid -> [("customer", toQueryParam cid)]) customerId
      <> maybe [] (\cid -> [("customer_email", toQueryParam cid)]) customerEmail
      <> maybe [] (\sub -> [("subscription", sub)]) subscription
      <> maybe [] (\b -> [("allow_promotion_codes", if b then "true" else "false")]) allowPromotionCodes
      <> maybe [] (toForm . discountsToForm) discounts
      -- Tricky
      <> case mode of
        CheckoutSetup -> [("currency", "USD")]
        _ -> toForm $ lineItemsToForm lineItems

-- | The Checkout Session object.
--
-- <https://docs.stripe.com/api/checkout/sessions/object>
data CheckoutSession = CheckoutSession
  { checkoutSessionId :: CheckoutSessionID,
    checkoutSessionCustomer :: Maybe StripeCustomerID,
    checkoutSessionPaymentStatus :: PaymentStatus,
    checkoutSessionUrl :: Maybe URI,
    checkoutSessionPaymentIntent :: Maybe PaymentIntentID,
    checkoutSessionAllowPromotionCodes :: Maybe Bool,
    checkoutSessionTotalDetails :: Maybe TotalDetails
  }
  deriving (Show, Generic, Eq)

instance FromJSON CheckoutSession where
  parseJSON = genericParseJSON $ customOptionsSnake "CheckoutSession"

instance ToJSON CheckoutSession where
  toJSON = genericToJSON $ customOptionsSnake "CheckoutSession"

instance GenValid CheckoutSession

instance Validity CheckoutSession

instance FromRow CheckoutSession

instance ToRow CheckoutSession

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

instance SQL.FromField PaymentStatus where
  fromField =
    SQL.fromField @Text >=> \case
      "paid" -> SQL.Ok Paid
      "unpaid" -> SQL.Ok Unpaid
      "no_payment_required" -> SQL.Ok NoPaymentRequired
      s -> fail $ "Cannot parse PaymentStatus " <> toString s

instance SQL.ToField PaymentStatus where
  toField = SQL.toField . snakeCase . show

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
