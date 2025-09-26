{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe where

import Data.Generics.Product (HasType (..))
import Relude
import Servant
import Servant.Client
import Stripe.Auth
import Stripe.Checkout
import Stripe.Customer
import Stripe.Price
import Stripe.Product
import Stripe.Subscription

type StripeAPI =
  StripeCheckoutAPI
    :<|> StripeCustomersAPI
    :<|> StripeSubscriptionsAPI
    :<|> StripeProductsAPI
    :<|> StripePricesAPI

stripeAPI :: Proxy StripeAPI
stripeAPI = Proxy

newtype StripeEnv = StripeEnv {unStripeEnv :: ClientEnv} deriving (Generic)

createStripeCheckoutSessionClient ::
  StripeAPIKey -> CreateCheckoutSession -> ClientM CheckoutSession
createStripeCustomerClient ::
  StripeAPIKey -> CreateCustomer -> ClientM StripeCustomer
createStripeSubscriptionClient ::
  StripeAPIKey -> CreateSubscription -> ClientM StripeSubscription
deleteStripeSubscriptionClient ::
  StripeAPIKey -> StripeSubscriptionID -> ClientM StripeSubscription
createStripeProductClient ::
  StripeAPIKey -> CreateProduct -> ClientM StripeProduct
getStripeProductClient ::
  StripeAPIKey -> StripeProductID -> ClientM StripeProduct
createStripePriceClient ::
  StripeAPIKey -> CreatePrice -> ClientM StripePrice
getStripePriceClient ::
  StripeAPIKey -> StripePriceID -> ClientM StripePrice
( createStripeCheckoutSessionClient
    :<|> createStripeCustomerClient
    :<|> (createStripeSubscriptionClient :<|> deleteStripeSubscriptionClient)
    :<|> (createStripeProductClient :<|> getStripeProductClient)
    :<|> (createStripePriceClient :<|> getStripePriceClient)
  ) = client stripeAPI

newtype StripeError = StripeError {unStripeError :: ClientError}
  deriving (Generic, Show)

createStripeCheckoutSession ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  CreateCheckoutSession ->
  m (Either StripeError CheckoutSession)
createStripeCheckoutSession params = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (createStripeCheckoutSessionClient auth params) env

createStripeSubscription ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  CreateSubscription ->
  m (Either StripeError StripeSubscription)
createStripeSubscription params = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (createStripeSubscriptionClient auth params) env

deleteStripeSubscription ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  StripeSubscriptionID ->
  m (Either StripeError StripeSubscription)
deleteStripeSubscription params = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (deleteStripeSubscriptionClient auth params) env

createStripeCustomer ::
  ( MonadIO m,
    MonadReader r m,
    HasType StripeEnv r,
    HasType StripeAPIKey r
  ) =>
  CreateCustomer ->
  m (Either StripeError StripeCustomer)
createStripeCustomer params = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (createStripeCustomerClient auth params) env

createStripeProduct ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  CreateProduct ->
  m (Either StripeError StripeProduct)
createStripeProduct params = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (createStripeProductClient auth params) env

getStripeProduct ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  StripeProductID ->
  m (Either StripeError StripeProduct)
getStripeProduct pid = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (getStripeProductClient auth pid) env

createStripePrice ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  CreatePrice ->
  m (Either StripeError StripePrice)
createStripePrice params = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (createStripePriceClient auth params) env

getStripePrice ::
  (MonadReader r m, HasType StripeEnv r, HasType StripeAPIKey r, MonadIO m) =>
  StripePriceID ->
  m (Either StripeError StripePrice)
getStripePrice pid = withStripeAuth $ \(env, auth) ->
  liftIO $ runClientM (getStripePriceClient auth pid) env

withStripeAuth ::
  ( MonadReader r m,
    HasType StripeEnv r,
    HasType StripeAPIKey r
  ) =>
  ((ClientEnv, StripeAPIKey) -> m (Either ClientError c)) ->
  m (Either StripeError c)
withStripeAuth f = do
  StripeEnv env <- asks getTyped
  StripeAPIKey apiKey <- asks getTyped
  -- Tricky
  let auth = StripeAPIKey $ "Bearer " <> apiKey
  first StripeError <$> curry f env auth
