{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.TextEnvelope.Certificates.GenesisKeyDelegationCertificate
  ( golden_sophieGenesisKeyDelegationCertificate
  ) where

import           Bcc.Api (AsType (..), HasTextEnvelope (..))
import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieGenesisKeyDelegationCertificate :: Property
golden_sophieGenesisKeyDelegationCertificate =
  propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
    -- Reference certificate
    referenceCertificateFilePath <-
      noteInputFile $
        "test/data/golden/sophie/certificates/"
          <> "genesis_key_delegation_certificate"

    -- Verification key and certificate filepaths
    genesisVerKeyFilePath <-
      noteTempFile tempDir "genesis-verification-key-file"
    genesisDelegVerKeyFilePath <-
      noteTempFile tempDir "genesis-delegate-verification-key-file"
    vrfVerKeyFilePath <- noteTempFile tempDir "vrf-verification-key-file"
    genesisKeyDelegCertFilePath <-
      noteTempFile tempDir "genesis-key-delegation-certificate-file"

    -- Signing Key filepaths
    genesisSignKeyFilePath <- noteTempFile tempDir "genesis-signing-key-file"
    genesisDelegSignKeyFilePath <- noteTempFile tempDir "genesis-delegate-signing-key-file"
    vrfSignKeyFilePath <- noteTempFile tempDir "vrf-signing-key-file"

    genesisDelegOpCertCounterFilePath <- noteTempFile tempDir "genesis-delegate-opcert-counter"


    -- Generate genesis key pair
    void $ execBccCLI
      [ "genesis","key-gen-genesis"
      , "--verification-key-file", genesisVerKeyFilePath
      , "--signing-key-file", genesisSignKeyFilePath
      ]

    -- Generate genesis delegate key pair
    void $ execBccCLI
      [ "genesis","key-gen-delegate"
      , "--verification-key-file", genesisDelegVerKeyFilePath
      , "--signing-key-file", genesisDelegSignKeyFilePath
      , "--operational-certificate-issue-counter-file"
      , genesisDelegOpCertCounterFilePath
      ]

    -- Generate VRF key pair
    void $ execBccCLI
      [ "node","key-gen-VRF"
      , "--verification-key-file", vrfVerKeyFilePath
      , "--signing-key-file", vrfSignKeyFilePath
      ]

    H.assertFilesExist
      [ genesisVerKeyFilePath
      , genesisDelegVerKeyFilePath
      , vrfVerKeyFilePath
      ]

    -- Create genesis key delegation certificate
    void $ execBccCLI
      [ "governance","create-genesis-key-delegation-certificate"
      , "--genesis-verification-key-file", genesisVerKeyFilePath
      , "--genesis-delegate-verification-key-file", genesisDelegVerKeyFilePath
      , "--vrf-verification-key-file", vrfVerKeyFilePath
      , "--out-file", genesisKeyDelegCertFilePath
      ]

    H.assertFilesExist [genesisKeyDelegCertFilePath]

    let certificateType = textEnvelopeType AsCertificate

    checkTextEnvelopeFormat
      certificateType
      referenceCertificateFilePath
      genesisKeyDelegCertFilePath
