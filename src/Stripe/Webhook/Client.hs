{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Stripe.Webhook.Client where

import Data.Aeson (encode)
import Data.ByteString.Lazy ()
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Relude
import Servant hiding (addHeader)
import Servant.API.ContentTypes ()
import Servant.Client
import Servant.Client.Core (addHeader)
import Servant.Client.Core.Request (setRequestBodyLBS)
import Stripe.Event (StripeEvent)
import Stripe.Webhook (StripeWebhookSecret, computeSignature)
import qualified Stripe.Webhook as Stripe

instance
  (HasClient m sub, MimeRender ct a) =>
  HasClient m (Stripe.StripeSignedReqBody mods (ct ': cts) a :> sub)
  where
  type
    Client m (Stripe.StripeSignedReqBody mods (ct ': cts) a :> sub) =
      StripeWebhookSecret -> UTCTime -> a -> Client m sub
  clientWithRoute pm _ req secret now webhookReq =
    let body = mimeRender (Proxy @ct) webhookReq
        sig = computeSignature secret now $ toStrict body
        timestamp = show @Text (round (utcTimeToPOSIXSeconds now) :: Integer)
        headerVal = "t=" <> timestamp <> ",v1=" <> sig
     in clientWithRoute pm (Proxy @sub)
          $ addHeader "Stripe-Signature" headerVal
          $ setRequestBodyLBS body (contentType $ Proxy @ct) req
  hoistClientMonad pm _ nt f a now =
    hoistClientMonad pm (Proxy @sub) nt . f a now

instance MimeRender JSON StripeEvent where
  mimeRender _ = encode
