{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Transaction.Assemble
  ( golden_sophieTransactionAssembleWitness_SigningKey
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- Check that we can assemble a txbody and a tx witness to form a transaction

golden_sophieTransactionAssembleWitness_SigningKey :: Property
golden_sophieTransactionAssembleWitness_SigningKey = propertyOnce $ H.moduleWorkspace "tmp" $ \tempDir -> do
  witnessTx <- noteTempFile tempDir "single-signing-key-witness-tx"
  txBodyFile <- noteInputFile "test/data/golden/sophie/tx/txbody"
  signingKeyWitnessFile <- noteInputFile "test/data/golden/sophie/witnesses/singleSigningKeyWitness"
  void $ execBccCLI
    [ "transaction","sign-witness"
    , "--tx-body-file", txBodyFile
    , "--witness-file", signingKeyWitnessFile
    , "--witness-file", signingKeyWitnessFile
    , "--out-file", witnessTx
    ]

  H.assertFileOccurences 1 "Tx JenEra" witnessTx
