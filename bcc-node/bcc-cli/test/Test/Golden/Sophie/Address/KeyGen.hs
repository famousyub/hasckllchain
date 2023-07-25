{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Address.KeyGen
  ( golden_sophieAddressKeyGen
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieAddressKeyGen :: Property
golden_sophieAddressKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteTempFile tempDir "address.vkey"
  addressSKeyFile <- noteTempFile tempDir "address.skey"

  void $ execBccCLI
    [ "address","key-gen"
    , "--verification-key-file", addressVKeyFile
    , "--signing-key-file", addressSKeyFile
    ]

  void $ H.readFile addressVKeyFile
  void $ H.readFile addressSKeyFile

  H.assertFileOccurences 1 "PaymentVerificationKeySophie" addressVKeyFile
  H.assertFileOccurences 1 "PaymentSigningKeySophie_ed25519" addressSKeyFile

  H.assertEndsWithSingleNewline addressVKeyFile
  H.assertEndsWithSingleNewline addressSKeyFile
