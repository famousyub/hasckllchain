{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Keys.GenesisDelegateKeys
  ( golden_sophieGenesisDelegateKeys
  ) where

import           Bcc.Api (AsType (..), HasTextEnvelope (..))
import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair & operational certificate counter file
--   2. Check for the existence of the key pair & counter file
--   3. Check the TextEnvelope serialization format has not changed.
golden_sophieGenesisDelegateKeys :: Property
golden_sophieGenesisDelegateKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/sophie/keys/genesis_delegate_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/sophie/keys/genesis_delegate_keys/signing_key"
  referenceOpCertCounter <- noteInputFile "test/data/golden/sophie/keys/genesis_delegate_keys/operational_certificate_counter"

  -- Key filepaths
  verKey <- noteTempFile tempDir "genesis-delegate-verification-key-file"
  signKey <- noteTempFile tempDir "genesis-delegate-signing-key-file"
  opCertCounter <- noteTempFile tempDir "delegate-operational-cert-counter-file"

  -- Generate payment verification key
  void $ execBccCLI
    [ "genesis","key-gen-delegate"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    , "--operational-certificate-issue-counter-file", opCertCounter
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsGenesisDelegateKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsGenesisDelegateKey)
      operationalCertCounterType = textEnvelopeType AsOperationalCertificateIssueCounter

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
  checkTextEnvelopeFormat operationalCertCounterType referenceOpCertCounter opCertCounter
