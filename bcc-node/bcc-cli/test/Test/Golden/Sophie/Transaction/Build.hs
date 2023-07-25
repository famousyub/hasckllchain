{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Transaction.Build
  ( golden_sophieTransactionBuild
  , golden_sophieTransactionBuild_TxInScriptWitnessed
  , golden_sophieTransactionBuild_Minting
  , golden_sophieTransactionBuild_CertificateScriptWitnessed
  , golden_sophieTransactionBuild_WithdrawalScriptWitnessed
  ) where

import           Bcc.Prelude
import           Prelude (String)

import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

txOut :: String
txOut = "addr1q94cxl99qvtwunsqqv6g9mgj3zrawtpt4edsgwxkjtwpy5dsezcht90tmwfur7t5hc9fk8hjd3r5vjwec2h8vmk3xh8s7er7t3+100"

txIn :: String
txIn = "2392d2b1200b5139fe555c81261697b29a8ccf561c5c783d46e78a479d977053#0"

golden_sophieTransactionBuild :: Property
golden_sophieTransactionBuild =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execBccCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyJen" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile


golden_sophieTransactionBuild_CertificateScriptWitnessed :: Property
golden_sophieTransactionBuild_CertificateScriptWitnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let deregcert = "test/data/golden/sophie/certificates/stake_address_deregistration_certificate"
        scriptWit = "test/data/golden/sophie/multisig/scripts/any"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execBccCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut
      , "--certificate-file", deregcert, "--certificate-script-file", scriptWit
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyJen" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile

golden_sophieTransactionBuild_Minting :: Property
golden_sophieTransactionBuild_Minting =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let scriptWit = "test/data/golden/sophie/multisig/scripts/any"

    polid <- execBccCLI
               [ "transaction"
               , "policyid"
               , "--script-file"
               , scriptWit
               ]

    let dummyMA = filter (/= '\n') $ "50 " ++ polid ++ ".ethereum"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execBccCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut ++ "+" ++ dummyMA, "--minting-script-file", scriptWit
      , "--mint", dummyMA
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyJen" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile

golden_sophieTransactionBuild_WithdrawalScriptWitnessed :: Property
golden_sophieTransactionBuild_WithdrawalScriptWitnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    stakeAddress <- H.readFile "test/data/golden/sophie/keys/stake_keys/reward_address"

    let withdrawal = filter (/= '\n') $ stakeAddress <> "+100"
        scriptWit = "test/data/golden/sophie/multisig/scripts/any"

    void $ execBccCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn
      , "--tx-out", txOut
      , "--withdrawal", withdrawal, "--withdrawal-script-file", scriptWit
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyJen" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile

golden_sophieTransactionBuild_TxInScriptWitnessed :: Property
golden_sophieTransactionBuild_TxInScriptWitnessed =
  propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
    let scriptWit = "test/data/golden/sophie/multisig/scripts/any"

    txBodyOutFile <- noteTempFile tempDir "tx-body-out"

    void $ execBccCLI
      [ "transaction","build-raw"
      , "--tx-in", txIn, "--txin-script-file", scriptWit
      , "--tx-out", txOut
      , "--fee", "12"
      , "--tx-body-file", txBodyOutFile
      ]

    H.assertFileOccurences 1 "TxBodyJen" txBodyOutFile

    H.assertEndsWithSingleNewline txBodyOutFile


