{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Webhook where

import Servant
import Stripe.Auth
import Stripe.Event
import Stripe.Subscription

type WebhookSignatureHeader =
  Header' '[Required] "Stripe-Signature" StripeSignature

type StripeWebhookAPI =
  "stripe"
    :> "webhook"
    :> WebhookSignatureHeader
    :> ReqBody '[JSON] (StripeEvent StripeSubscription)
    :> Post '[JSON] NoContent
