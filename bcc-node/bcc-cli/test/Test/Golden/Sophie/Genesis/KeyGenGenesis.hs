{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Genesis.KeyGenGenesis
  ( golden_sophieGenesisKeyGenGenesis
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieGenesisKeyGenGenesis :: Property
golden_sophieGenesisKeyGenGenesis = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteTempFile tempDir "key-gen.vkey"
  signingKeyFile <- noteTempFile tempDir "key-gen.skey"

  void $ execBccCLI
    [ "genesis","key-gen-genesis"
    , "--verification-key-file", verificationKeyFile
    , "--signing-key-file", signingKeyFile
    ]

  H.assertFileOccurences 1 "GenesisVerificationKey_ed25519" verificationKeyFile
  H.assertFileOccurences 1 "GenesisSigningKey_ed25519" signingKeyFile

  H.assertEndsWithSingleNewline verificationKeyFile
  H.assertEndsWithSingleNewline signingKeyFile
