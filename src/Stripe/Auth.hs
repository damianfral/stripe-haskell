{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Auth where

import Relude
import Servant.API

newtype StripeAPIKey = StripeAPIKey {unStripeAPIKey :: Text}
  deriving (Generic)
  deriving newtype (ToHttpApiData)

type StripeAuthHeader = Header' '[Required] "Authorization" StripeAPIKey

newtype StripeSignature = StripeSignature {unStripeSignature :: Text}
  deriving newtype (FromHttpApiData)
