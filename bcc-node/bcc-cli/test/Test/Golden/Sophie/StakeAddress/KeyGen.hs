{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.StakeAddress.KeyGen
  ( golden_sophieStakeAddressKeyGen
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieStakeAddressKeyGen :: Property
golden_sophieStakeAddressKeyGen = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "kes.vkey"
  signingKeyFile <- noteTempFile tempDir "kes.skey"

  void $ execBccCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    ]

  H.assertFileOccurences 1 "StakeVerificationKeySophie_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "StakeSigningKeySophie_ed25519" signingKeyFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
