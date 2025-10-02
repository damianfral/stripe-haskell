{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.EmailAddress (EmailAddress, emailToText) where

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Database.SQLite.Simple.FromField
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import Relude
import Servant (ToHttpApiData)
import Servant.API (ToHttpApiData (toUrlPiece))
import Text.Email.Validate as E

instance Validity EmailAddress

instance GenValid EmailAddress

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
