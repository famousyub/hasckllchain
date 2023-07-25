{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Transaction.Sign
  ( golden_sophieTransactionSign
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieTransactionSign :: Property
golden_sophieTransactionSign = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  txBodyFile <- noteInputFile "test/data/golden/sophie/tx/txbody"
  initialUtxo1SigningKeyFile <- noteInputFile "test/data/golden/sophie/keys/payment_keys/signing_key"
  utxoSigningKeyFile <- noteInputFile "test/data/golden/sophie/transaction-sign/utxo.skey"
  stakeSigningKeyFile <- noteInputFile "test/data/golden/sophie/transaction-sign/stake.skey"
  nodeColdSigningKeyFile <- noteInputFile "test/data/golden/sophie/transaction-sign/node-cold.skey"
  signedTransactionFile <- noteTempFile tempDir "signed.tx"
  transactionPoolRegSignedFile <- noteTempFile tempDir "tx-pool-reg.signed"

  -- Defaults to signing a Mainnet transaction

  void $ execBccCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--tx-file", signedTransactionFile
    ]

  H.assertFileOccurences 1 "Tx JenEra" signedTransactionFile
  H.assertEndsWithSingleNewline signedTransactionFile

  -- Sign for a testnet with a testnet network magic of 11, but use two signing keys

  void $ execBccCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--signing-key-file", initialUtxo1SigningKeyFile
    , "--tx-file", signedTransactionFile
    ]

  H.assertFileOccurences 1 "Tx JenEra" signedTransactionFile
  H.assertEndsWithSingleNewline signedTransactionFile

  -- Sign a pool registration transaction.
  -- TODO: This needs to use an unsigned tx with a registration certificate

  void $ execBccCLI
    [ "transaction","sign"
    , "--mainnet"
    , "--tx-body-file", txBodyFile
    , "--signing-key-file", utxoSigningKeyFile
    , "--signing-key-file", stakeSigningKeyFile
    , "--signing-key-file", nodeColdSigningKeyFile
    , "--tx-file", transactionPoolRegSignedFile
    ]

  H.assertFileOccurences 1 "Tx JenEra" transactionPoolRegSignedFile
  H.assertEndsWithSingleNewline transactionPoolRegSignedFile
