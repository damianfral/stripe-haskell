{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Helpers where

import Data.Aeson
import Data.Aeson.Casing
import Data.Char (toLower)
import Relude
import Web.FormUrlEncoded (FormOptions (FormOptions))

lower1 :: [Char] -> [Char]
lower1 (c : cs) = toLower c : cs
lower1 [] = []

customOptions :: String -> Options
customOptions prefix =
  defaultOptions
    { fieldLabelModifier = lower1 . drop (length prefix),
      omitNothingFields = True
    }

customOptionsLower :: String -> Options
customOptionsLower prefix =
  (customOptions prefix)
    { fieldLabelModifier = fmap toLower . drop (length prefix)
    }

customOptionsSnake :: String -> Options
customOptionsSnake prefix =
  (customOptions prefix)
    { fieldLabelModifier = snakeCase . drop (length prefix)
    }

formOptionsSnake :: String -> FormOptions
formOptionsSnake prefix = FormOptions $ snakeCase . drop (length prefix)
