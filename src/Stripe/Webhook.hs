{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stripe.Webhook where

import Crypto.Hash
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import Crypto.Util (constTimeEq)
import Data.Aeson
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.CaseInsensitive as CI
import Data.Data (typeRep)
import Data.Kind
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (hContentType)
import Network.Wai (lazyRequestBody, requestHeaders)
import Relude
import Servant
import Servant.API.ContentTypes
import Servant.API.Modifiers
import Servant.Server.Internal.Delayed (addBodyCheck)
import Servant.Server.Internal.DelayedIO (delayedFail, delayedFailFatal, withRequest)
import Servant.Server.Internal.ErrorFormatter
import Stripe.Auth
import Stripe.Event
import Stripe.Subscription

type WebhookSignatureHeader =
  Header' '[Required] "Stripe-Signature" StripeSignature

data
  StripeSignedReqBody
    (mods :: [Type])
    (contentTypes :: [Type])
    (a :: Type)
  deriving (Typeable)

type StripeWebhookAPI =
  "stripe"
    :> "webhook"
    :> WebhookSignatureHeader
    :> StripeSignedReqBody '[Required] '[JSON] (StripeEvent StripeSubscription)
    :> Post '[JSON] NoContent

newtype StripeWebhookSecret = StripeWebhookSecret {unStripeWebhookSecret :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (FromHttpApiData, ToHttpApiData)

instance
  ( AllCTUnrender list a,
    HasServer api context,
    SBoolI (FoldLenient mods),
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    HasContextEntry context StripeWebhookSecret
  ) =>
  HasServer (StripeSignedReqBody mods list a :> api :: Type) context
  where
  type
    ServerT (StripeSignedReqBody mods list a :> api) m =
      If (FoldLenient mods) (Either String a) a -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context
      $ addBodyCheck subserver ctCheck bodyCheck
    where
      rep = typeRep (Proxy :: Proxy ReqBody')
      formatError =
        bodyParserErrorFormatter
          $ getContextEntry
          $ mkContextWithErrorFormatter context

      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH =
              fromMaybe "application/octet-stream"
                $ Map.lookup hContentType
                $ fromList
                $ requestHeaders request
        let canHandleResult :: Maybe (LByteString -> Either String a) =
              canHandleCTypeH (Proxy :: Proxy list) (fromStrict contentTypeH)
        case canHandleResult of
          Nothing -> delayedFail err415
          Just f -> pure f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \request -> do
        body <- liftIO (lazyRequestBody request)
        let secret = getContextEntry context
        let mSignatureHeader =
              Map.lookup
                (CI.mk "Stripe-Signature")
                (fromList $ requestHeaders request)
        case mSignatureHeader of
          Nothing ->
            delayedFailFatal
              err401 {errReasonPhrase = "Missing Stripe-Signature header"}
          Just sigHeader -> do
            isValid <- do
              let signatureHeader = decodeUtf8With lenientDecode sigHeader
              liftIO $ isValidSignature secret (toStrict body) signatureHeader
            if isValid
              then do
                let mrqbody = f body
                case sbool :: SBool (FoldLenient mods) of
                  STrue -> pure mrqbody
                  SFalse -> case mrqbody of
                    Left e -> delayedFailFatal $ formatError rep request e
                    Right v -> pure v
              else
                delayedFailFatal err401 {errReasonPhrase = "Invalid signature"}

-- | Verify a Stripe webhook signature.
-- See https://stripe.com/docs/webhooks/signatures
isValidSignature ::
  -- | The signing secret.
  StripeWebhookSecret ->
  -- | The raw request body.
  ByteString ->
  -- | The value of the `Stripe-Signature` header.
  Text ->
  -- | Whether the signature is valid.
  IO Bool
isValidSignature secret body sigHeader = do
  let headerMap = parseSignatureHeader sigHeader
  case (Map.lookup "t" headerMap, Map.lookup "v1" headerMap) of
    (Just t, Just v1) -> do
      now <- getPOSIXTime
      let mTimestamp :: Maybe Int64 = readMaybe $ T.unpack t
      case mTimestamp of
        Nothing -> pure False
        Just timestamp ->
          -- Check if the timestamp is within the tolerance (e.g., 5 minutes)
          if abs (round now - timestamp) > 300
            then pure False
            -- Use a constant-time comparison function to prevent timing attacks
            else do
              let expectedSignature = computeSignature secret t body
              pure
                $ constTimeEq (T.encodeUtf8 v1) (T.encodeUtf8 expectedSignature)
    _ -> pure False

parseSignatureHeader :: Text -> Map.Map Text Text
parseSignatureHeader = Map.fromList . mapMaybe toPair . T.splitOn ","
  where
    toPair :: Text -> Maybe (Text, Text)
    toPair t = case T.splitOn "=" t of
      [k, v] -> Just (k, v)
      _ -> Nothing

computeSignature :: StripeWebhookSecret -> Text -> ByteString -> Text
computeSignature secret timestamp body =
  let signedPayload = T.encodeUtf8 timestamp <> "." <> body
      secretBS = T.encodeUtf8 (unStripeWebhookSecret secret)
      digest = hmac secretBS signedPayload :: HMAC SHA256
   in decodeUtf8With lenientDecode $ convertToBase Base16 (hmacGetDigest digest)

instance (HasLink sub) => HasLink (StripeSignedReqBody mods list a :> sub) where
  type MkLink (StripeSignedReqBody mods list a :> sub) r = MkLink sub r
  toLink toA _ = toLink toA $ Proxy @sub
