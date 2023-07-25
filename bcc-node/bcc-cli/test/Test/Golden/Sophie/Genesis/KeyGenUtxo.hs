{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Genesis.KeyGenUtxo
  ( golden_sophieGenesisKeyGenUtxo
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieGenesisKeyGenUtxo :: Property
golden_sophieGenesisKeyGenUtxo = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  utxoVerificationKeyFile <- noteTempFile tempDir "utxo.vkey"
  utxoSigningKeyFile <- noteTempFile tempDir "utxo.skey"

  void $ execBccCLI
    [ "genesis","key-gen-utxo"
    , "--verification-key-file", utxoVerificationKeyFile
    , "--signing-key-file", utxoSigningKeyFile
    ]

  H.assertFileOccurences 1 "GenesisUTxOVerificationKey_ed25519" utxoVerificationKeyFile
  H.assertFileOccurences 1 "GenesisUTxOSigningKey_ed25519" utxoSigningKeyFile

  H.assertEndsWithSingleNewline utxoVerificationKeyFile
  H.assertEndsWithSingleNewline utxoSigningKeyFile
