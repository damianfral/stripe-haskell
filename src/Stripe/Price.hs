{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Price where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (toLower)
import Data.GenValidity
import Data.Text (pack)
import Data.Time.Clock.POSIX (POSIXTime)
import Relude
import Servant
import Stripe.Auth
import Stripe.Product hiding (toFormArray, toFormObject)
import Web.FormUrlEncoded (ToForm (..))

-- | The ID of a Stripe Price.
--
-- <https://docs.stripe.com/api/prices/object#price_object-id>
newtype StripePriceID = StripePriceID {unStripePriceID :: Text}
  deriving newtype (FromJSON, ToJSON, ToHttpApiData)
  deriving stock (Show, Eq, Generic)

instance Validity StripePriceID

instance GenValid StripePriceID

-- | The Stripe Price object.
--
-- <https://docs.stripe.com/api/prices/object>
data StripePrice = StripePrice
  { stripePriceId :: StripePriceID,
    stripePriceActive :: Bool,
    stripePriceCurrency :: Text,
    stripePriceMetadata :: Object,
    stripePriceNickname :: Maybe Text,
    stripePriceProduct :: StripeProductID,
    stripePriceRecurring :: Maybe Object,
    stripePriceTaxBehavior :: Maybe Text,
    stripePriceObject :: Text,
    stripePriceBillingScheme :: Text,
    stripePriceCreated :: POSIXTime,
    stripePriceLivemode :: Bool,
    stripePriceLookupKey :: Maybe Text,
    stripePriceTiersMode :: Maybe Text,
    stripePriceTransformQuantity :: Maybe Object,
    stripePricePriceType :: Text,
    stripePriceUnitAmount :: Maybe Int,
    stripePriceUnitAmountDecimal :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Validity StripePrice

instance GenValid StripePrice

instance FromJSON StripePrice where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "stripePricePriceType" -> "type"
            other -> snakeCase . drop (length @[] "StripePrice") $ other
        }

instance ToJSON StripePrice where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "stripePricePriceType" -> "type"
            other -> snakeCase . drop (length @[] "StripePrice") $ other
        }

-- https://stripe.com/docs/api/prices/create

-- | Parameters for creating a Price.
--
-- <https://docs.stripe.com/api/prices/create>
data CreatePrice = CreatePrice
  { createPriceCurrency :: Text,
    createPriceProduct :: StripeProductID,
    createPriceActive :: Maybe Bool,
    createPriceMetadata :: Maybe Object,
    createPriceNickname :: Maybe Text,
    createPriceRecurring :: Maybe Object,
    createPriceTaxBehavior :: Maybe Text,
    createPriceLookupKey :: Maybe Text,
    createPriceTiers :: Maybe [Object],
    createPriceTiersMode :: Maybe Text,
    createPriceTransferLookupKey :: Maybe Bool,
    createPriceTransformQuantity :: Maybe Object,
    createPriceUnitAmount :: Maybe Int,
    createPriceUnitAmountDecimal :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreatePrice where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = camelTo2 '_' . drop (length @[] "CreatePrice")
        }

toFormObject :: Text -> Object -> [(Text, Text)]
toFormObject prefix obj = concatMap (go . first show) (KeyMap.toList obj)
  where
    go (k, v) = toFormValue (prefix <> "[" <> k <> "]") v
    toFormValue :: Text -> Value -> [(Text, Text)]
    toFormValue key (String val) = [(key, val)]
    toFormValue key (Number val) = [(key, pack . show $ val)]
    toFormValue key (Bool val) = [(key, pack . fmap toLower . show $ val)]
    toFormValue key (Object o) = toFormObject key o
    toFormValue _key Null = []
    toFormValue _ (Array _) = error "form encoding of array not implemented"

toFormArrayOfObjects :: Text -> [Object] -> [(Text, Text)]
toFormArrayOfObjects prefix values =
  concat
    $ zipWith (\ix val -> toFormObject (prefix <> "[" <> show @Text @Int ix <> "]") val) [0 ..] values

instance ToForm CreatePrice where
  toForm CreatePrice {..} =
    toForm @[(Text, Text)]
      ( catMaybes
          [ Just ("currency", createPriceCurrency),
            Just ("product", unStripeProductID createPriceProduct),
            ("active",) . pack . fmap toLower . show <$> createPriceActive,
            ("nickname",) <$> createPriceNickname,
            ("tax_behavior",) <$> createPriceTaxBehavior,
            ("lookup_key",) <$> createPriceLookupKey,
            ("tiers_mode",) <$> createPriceTiersMode,
            ("transfer_lookup_key",) . pack . fmap toLower . show <$> createPriceTransferLookupKey,
            ("unit_amount",) . pack . show <$> createPriceUnitAmount,
            ("unit_amount_decimal",) <$> createPriceUnitAmountDecimal
          ]
      )
      <> maybe mempty (toForm . toFormObject "metadata") createPriceMetadata
      <> maybe mempty (toForm . toFormObject "recurring") createPriceRecurring
      <> maybe mempty (toForm . toFormArrayOfObjects "tiers") createPriceTiers
      <> maybe mempty (toForm . toFormObject "transform_quantity") createPriceTransformQuantity

type StripePricesAPI =
  "v1"
    :> "prices"
    :> ( StripeAuthHeader
           :> ReqBody '[FormUrlEncoded] CreatePrice
           :> Post '[JSON] StripePrice
           :<|> StripeAuthHeader
             :> Capture "id" StripePriceID
             :> Get '[JSON] StripePrice
       )
