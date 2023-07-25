{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Tx.TxBody
  ( golden_sophieTxBody
  ) where

import           Bcc.Api (AsType (..), HasTextEnvelope (..))
import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. We create a 'TxBody Sophie' file.
--   2. Check the TextEnvelope serialization format has not changed.
golden_sophieTxBody :: Property
golden_sophieTxBody = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceTxBody <- noteInputFile "test/data/golden/sophie/tx/txbody"

  -- Key filepaths
  transactionBodyFile <- noteTempFile tempDir "transaction-body-file"

  -- Create transaction body
  void $ execBccCLI
    [ "transaction", "build-raw"
    , "--tx-in", "91999ea21177b33ebe6b8690724a0c026d410a11ad7521caa350abdafa5394c3#0"
    , "--tx-out", "addr1v9wmu83pzajplrtpsq6tsqdgwr98x888trpmah2u0ezznsge7del3+100000000"
    , "--fee", "1000000"
    , "--invalid-hereafter", "500000"
    , "--out-file", transactionBodyFile
    ]

  let txBodyType = textEnvelopeType AsJenTxBody

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat txBodyType referenceTxBody transactionBodyFile
