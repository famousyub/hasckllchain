{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Keys.VestedDelegateKeys
  ( golden_sophieVestedDelegateKeys
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
golden_sophieVestedDelegateKeys :: Property
golden_sophieVestedDelegateKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/sophie/keys/vested_delegate_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/sophie/keys/vested_delegate_keys/signing_key"
  referenceOpCertCounter <- noteInputFile "test/data/golden/sophie/keys/vested_delegate_keys/operational_certificate_counter"

  -- Key filepaths
  verKey <- noteTempFile tempDir "vested-delegate-verification-key-file"
  signKey <- noteTempFile tempDir "vested-delegate-signing-key-file"
  opCertCounter <- noteTempFile tempDir "delegate-operational-cert-counter-file"

  -- Generate payment verification key
  void $ execBccCLI
    [ "genesis","key-gen-vesteddelegate"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    , "--operational-certificate-issue-counter-file", opCertCounter
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsVestedDelegateKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsVestedDelegateKey)
      operationalCertCounterType = textEnvelopeType AsOperationalCertificateIssueCounter

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
  checkTextEnvelopeFormat operationalCertCounterType referenceOpCertCounter opCertCounter