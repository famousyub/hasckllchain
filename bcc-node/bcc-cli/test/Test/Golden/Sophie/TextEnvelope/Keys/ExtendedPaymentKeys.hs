{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Keys.ExtendedPaymentKeys
  ( golden_sophieExtendedPaymentKeys
  ) where

import           Bcc.Api (AsType (..), HasTextEnvelope (..))
import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate a key pair
--   2. Check for the existence of the key pair
--   3. Check the TextEnvelope serialization format has not changed.
golden_sophieExtendedPaymentKeys :: Property
golden_sophieExtendedPaymentKeys = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceVerKey <- noteInputFile "test/data/golden/sophie/keys/extended_payment_keys/verification_key"
  referenceSignKey <- noteInputFile "test/data/golden/sophie/keys/extended_payment_keys/signing_key"

  -- Key filepaths
  verKey <- noteTempFile tempDir "extended-payment-verification-key-file"
  signKey <- noteTempFile tempDir "extended-payment-signing-key-file"

  -- Generate payment verification key
  void $ execBccCLI
    [ "address","key-gen"
    , "--extended-key"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  let signingKeyType = textEnvelopeType (AsSigningKey AsPaymentExtendedKey)
      verificationKeyType = textEnvelopeType (AsVerificationKey AsPaymentExtendedKey)

  -- Check the newly created files have not deviated from the
  -- golden files
  checkTextEnvelopeFormat verificationKeyType referenceVerKey verKey
  checkTextEnvelopeFormat signingKeyType referenceSignKey signKey
