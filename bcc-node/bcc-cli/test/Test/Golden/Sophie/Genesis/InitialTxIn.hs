{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Genesis.InitialTxIn
  ( golden_sophieGenesisInitialTxIn
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieGenesisInitialTxIn :: Property
golden_sophieGenesisInitialTxIn = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  verificationKeyFile <- noteInputFile "test/data/golden/sophie/keys/genesis_verification_keys/genesis-utxo.vkey"
  goldenUtxoHashFile <- noteInputFile "test/data/golden/sophie/keys/genesis_utxo_hashes/utxo_hash"
  utxoHashFile <- noteTempFile tempDir "utxo_hash"

  utxoHash <- execBccCLI
    [ "genesis","initial-txin"
    , "--testnet-magic", "16"
    , "--verification-key-file", verificationKeyFile
    ]

  H.writeFile utxoHashFile utxoHash

  goldenUtxoHash <- H.readFile goldenUtxoHashFile

  equivalence utxoHash goldenUtxoHash
