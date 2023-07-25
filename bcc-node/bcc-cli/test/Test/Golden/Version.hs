{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Version
  ( golden_version
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

{- HLINT ignore "Use camelCase" -}

golden_version :: Property
golden_version = propertyOnce $ do
  void $ execBccCLI
    [ "version"
    ]
