{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Product where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (toLower)
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Time ()
import Data.Text (pack)
import Data.Time.Clock.POSIX (POSIXTime)
import Relude
import Servant
import Stripe.Auth (StripeAuthHeader)
import Web.FormUrlEncoded (ToForm (..), toForm)

-- | The ID of a Stripe Product.
--
-- <https://docs.stripe.com/api/products/object#product_object-id>
newtype StripeProductID = StripeProductID {unStripeProductID :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, ToHttpApiData)

instance Validity StripeProductID

instance GenValid StripeProductID

-- | The dimensions of a product package.
--
-- <https://docs.stripe.com/api/products/object#product_object-package_dimensions>
data PackageDimensions = PackageDimensions
  { packageDimensionsHeight :: Double,
    packageDimensionsWidth :: Double,
    packageDimensionsLength :: Double,
    packageDimensionsWeight :: Double
  }
  deriving (Show, Eq, Generic)

instance Validity PackageDimensions

instance GenValid PackageDimensions where
  genValid = do
    h <- genValid @Int
    w <- genValid @Int
    l <- genValid @Int
    we <- genValid @Int
    pure
      $ PackageDimensions
        (fromIntegral h / 100)
        (fromIntegral w / 100)
        (fromIntegral l / 100)
        (fromIntegral we / 100)

instance FromJSON PackageDimensions where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier =
            snakeCase . drop (length @[] "PackageDimensions")
        }

instance ToJSON PackageDimensions where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier =
            snakeCase . drop (length @[] "PackageDimensions")
        }

-- https://stripe.com/docs/api/products/object

-- | The Stripe Product object.
--
-- <https://docs.stripe.com/api/products/object>
data StripeProduct = StripeProduct
  { stripeProductId :: StripeProductID,
    stripeProductName :: Text,
    stripeProductActive :: Bool,
    stripeProductCreated :: POSIXTime,
    stripeProductUpdated :: POSIXTime,
    stripeProductDescription :: Maybe Text,
    stripeProductDefaultPrice :: Maybe Text,
    stripeProductImages :: [Text],
    stripeProductLivemode :: Bool,
    stripeProductMetadata :: Object,
    stripeProductPackageDimensions :: Maybe PackageDimensions,
    stripeProductShippable :: Maybe Bool,
    stripeProductStatementDescriptor :: Maybe Text,
    stripeProductTaxCode :: Maybe Text,
    stripeProductUnitLabel :: Maybe Text,
    stripeProductUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Validity StripeProduct

instance GenValid StripeProduct

instance FromJSON StripeProduct where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = snakeCase . drop (length @[] "stripeProduct")}

instance ToJSON StripeProduct where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier =
            snakeCase . drop (length @[] "StripeProduct")
        }

-- | Parameters for creating a Product.
--
-- <https://docs.stripe.com/api/products/create>
data CreateProduct = CreateProduct
  { createProductName :: Text,
    createProductActive :: Maybe Bool,
    createProductDescription :: Maybe Text,
    createProductId :: Maybe StripeProductID,
    createProductMetadata :: Maybe Object,
    createProductTaxCode :: Maybe Text,
    createProductImages :: Maybe [Text],
    createProductPackageDimensions :: Maybe Object,
    createProductShippable :: Maybe Bool,
    createProductStatementDescriptor :: Maybe Text,
    createProductUnitLabel :: Maybe Text,
    createProductUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateProduct where
  toJSON =
    genericToJSON
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = camelTo2 '_' . drop (length @[] "createProduct")
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

toFormArray :: Text -> [Text] -> [(Text, Text)]
toFormArray prefix values =
  let keys = fmap (\ix -> prefix <> "[" <> show @Text @Int ix <> "]") [0 ..]
   in zip keys values

instance ToForm CreateProduct where
  toForm CreateProduct {..} =
    toForm @[(Text, Text)]
      ( catMaybes
          [ Just ("name", createProductName),
            ("active",) . pack . fmap toLower . show <$> createProductActive,
            ("description",) <$> createProductDescription,
            ("id",) . unStripeProductID <$> createProductId,
            ("tax_code",) <$> createProductTaxCode,
            ("shippable",) . pack . fmap toLower . show <$> createProductShippable,
            ("statement_descriptor",) <$> createProductStatementDescriptor,
            ("unit_label",) <$> createProductUnitLabel,
            ("url",) <$> createProductUrl
          ]
      )
      <> maybe mempty (toForm . toFormArray "images") createProductImages
      <> maybe mempty (toForm . toFormObject "metadata") createProductMetadata
      <> maybe mempty (toForm . toFormObject "package_dimensions") createProductPackageDimensions

type StripeProductsAPI =
  "v1"
    :> "products"
    :> ( StripeAuthHeader
           :> ReqBody '[FormUrlEncoded] CreateProduct
           :> Post '[JSON] StripeProduct
           :<|> StripeAuthHeader
             :> Capture "id" StripeProductID
             :> Get '[JSON] StripeProduct
       )
