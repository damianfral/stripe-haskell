{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.WebhookSpec (spec) where

import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types.Status (status401)
import Relude
import Servant
import Servant.Client
import Stripe.Event (StripeEvent)
import Stripe.Webhook
import Stripe.Webhook.Client ()
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Validity

spec :: Spec
spec = do
  let secret = StripeWebhookSecret "test-secret"
  let apiProxy = (Proxy @StripeWebhookAPI)
  servantSpecWithContext apiProxy (secret :. EmptyContext) server $ do
    it "should return 200 on valid signature" $ \clientEnv -> do
      forAllValid $ \(webhookReq :: StripeEvent) -> do
        let client' = client $ Proxy @StripeWebhookAPI
        now <- liftIO getCurrentTime
        res <- runClientM (client' secret now webhookReq) clientEnv
        res `shouldBe` Right NoContent

    it "should return 401 on invalid signature" $ \clientEnv -> do
      forAllValid $ \(webhookReq :: StripeEvent) -> do
        let client' = client $ Proxy @StripeWebhookAPI
        let badSecret = StripeWebhookSecret "bad-secret"
        now <- liftIO getCurrentTime
        res <- runClientM (client' badSecret now webhookReq) clientEnv
        case res of
          Left (FailureResponse _ response) ->
            responseStatusCode response `shouldBe` status401
          _ -> expectationFailure "Expected a 401 failure"

server :: ServerT StripeWebhookAPI Handler
server _ = pure NoContent
