{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.EmailAddress (EmailAddress, module Data.EmailAddress) where

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Database.SQLite.Simple.FromField
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import Relude
import Servant (ToHttpApiData)
import Servant.API (ToHttpApiData (toUrlPiece))
import Test.QuickCheck (elements)
import Text.Email.Validate as E

instance Validity EmailAddress where
  validate = trivialValidation . E.isValid . toByteString

instance GenValid EmailAddress where
  shrinkValid = pure []
  genValid = do
    lPart <- genLocal
    dPart <- genDomain
    case E.validate (encodeUtf8 (lPart <> "@" <> dPart)) of
      Left _ -> genValid -- retry until valid
      Right v -> pure v
    where
      genLocal = do
        -- allow alphanumerics + a few safe chars
        xs <- genListOf1 $ elements charList
        pure (toText xs)
      genDomain = do
        labels <- genListOf1 $ genListOf1 $ elements charList2
        pure $ toText $ intercalate "." labels
      charList = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "._"
      charList2 = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

instance ToJSON EmailAddress where
  toJSON = toJSON @Text . decodeUtf8With lenientDecode . toByteString

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ \t ->
    case E.validate $ encodeUtf8 t of
      Left e -> fail e
      Right v -> pure v

instance ToHttpApiData EmailAddress where
  toUrlPiece = emailToText

emailToText :: EmailAddress -> Text
emailToText = decodeUtf8With lenientDecode . toByteString

instance SQL.FromField EmailAddress where
  fromField =
    fromField >=> \bs ->
      case E.validate bs of
        Left e -> fail e
        Right email -> pure email

instance SQL.ToField EmailAddress where
  toField = SQL.toField . toByteString
