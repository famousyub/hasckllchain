{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Genesis.KeyHash
  ( golden_sophieGenesisKeyHash
  ) where

import           Bcc.Prelude
import           Hedgehog (Property, (===))
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieGenesisKeyHash :: Property
golden_sophieGenesisKeyHash = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  referenceVerificationKey <- noteInputFile "test/data/golden/sophie/keys/genesis_keys/verification_key"
  goldenGenesisVerificationKeyHashFile <- noteInputFile "test/data/golden/sophie/keys/genesis_keys/verification_key.key-hash"
  genesisVerificationKeyHashFile <- noteTempFile tempDir "key-hash.hex"

  genesisVerificationKeyHash <- execBccCLI
    [ "genesis","key-hash"
    , "--verification-key-file", referenceVerificationKey
    ]

  H.writeFile genesisVerificationKeyHashFile genesisVerificationKeyHash

  goldenGenesisVerificationKeyHash <- H.readFile goldenGenesisVerificationKeyHashFile

  genesisVerificationKeyHash === goldenGenesisVerificationKeyHash
