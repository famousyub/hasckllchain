{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Certificates.MIRCertificate
  ( golden_sophieMIRCertificate
  ) where

import           Bcc.Api (AsType (..), HasTextEnvelope (..))
import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

-- | 1. Generate stake key pair
--   2. Create MIR certificate
--   s. Check the TextEnvelope serialization format has not changed.
golden_sophieMIRCertificate :: Property
golden_sophieMIRCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  -- Reference keys
  referenceMIRCertificate <- noteInputFile "test/data/golden/sophie/certificates/mir_certificate"

  -- Key filepaths
  verKey <- noteTempFile tempDir "stake-verification-key-file"
  signKey <- noteTempFile tempDir "stake-signing-key-file"
  mirCertificate <- noteTempFile tempDir "mir-certificate-file"

  -- Generate stake key pair
  void $ execBccCLI
    [ "stake-address","key-gen"
    , "--verification-key-file", verKey
    , "--signing-key-file", signKey
    ]

  H.assertFilesExist [verKey, signKey]

  let testAddr = "stake1u9j6axhcpd0exvrthn5dqzqt54g85akqvkn4uqmccm70qsc5hpv9w"
  -- Create MIR certificate
  void $ execBccCLI
    [ "governance","create-mir-certificate"
    , "--reserves" --TODO: Should also do "--reserves"
    , "--stake-address", testAddr
    , "--reward", "1000"
    , "--out-file", mirCertificate
    ]

  H.assertFilesExist [mirCertificate]

  let registrationCertificateType = textEnvelopeType AsCertificate

  checkTextEnvelopeFormat registrationCertificateType referenceMIRCertificate mirCertificate
