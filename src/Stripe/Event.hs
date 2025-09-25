{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Event where

import Data.Aeson
import Relude

data StripeEvent a = StripeEvent
  { stripeEventId :: Text,
    stripeEventType :: Text,
    stripeEventObject :: a
  }
  deriving (Generic, Show, Eq)

instance (FromJSON a) => FromJSON (StripeEvent a) where
  parseJSON = withObject "StripeEvent" $ \obj -> do
    eID <- obj .: "id"
    eType <- obj .: "type"
    eData <- obj .: "data"
    eObject <- eData .: "object"
    pure $ StripeEvent eID eType eObject
