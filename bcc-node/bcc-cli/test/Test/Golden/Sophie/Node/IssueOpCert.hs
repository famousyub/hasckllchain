{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Node.IssueOpCert
  ( golden_sophieNodeIssueOpCert
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified System.Directory as IO

{- HLINT ignore "Use camelCase" -}

golden_sophieNodeIssueOpCert :: Property
golden_sophieNodeIssueOpCert = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  hotKesVerificationKeyFile <- noteInputFile "test/data/golden/sophie/keys/kes_keys/verification_key"
  coldSigningKeyFile <- noteInputFile "test/data/golden/sophie/keys/genesis_delegate_keys/signing_key"
  originalOperationalCertificateIssueCounterFile <- noteInputFile "test/data/golden/sophie/keys/genesis_delegate_keys/operational_certificate_counter"
  operationalCertificateIssueCounterFile <- noteTempFile tempDir "delegate-op-cert.counter"
  operationalCertFile <- noteTempFile tempDir "operational.cert"

  void . liftIO $ IO.copyFile originalOperationalCertificateIssueCounterFile operationalCertificateIssueCounterFile

  -- We could generate the required keys here, but then if the KES generation fails this
  -- test would also fail which is misleading.
  -- However, the keys can be generated eg:
  --    cabal run bcc-cli:bcc-cli -- sophie node key-gen-KES \
  --        --verification-key-file bcc-cli/test/cli/node-issue-op-cert/data/node-kes.vkey \
  --        --signing-key-file /dev/null
  void $ execBccCLI
    [ "node","issue-op-cert"
    , "--hot-kes-verification-key-file", hotKesVerificationKeyFile
    , "--cold-signing-key-file", coldSigningKeyFile
    , "--operational-certificate-issue-counter", operationalCertificateIssueCounterFile
    , "--kes-period", "0"
    , "--out-file", operationalCertFile
    ]

  H.assertFileOccurences 1 "NodeOperationalCertificate" operationalCertFile
  H.assertFileOccurences 1 "Next certificate issue number: 1" operationalCertificateIssueCounterFile

  H.assertEndsWithSingleNewline operationalCertFile
  H.assertEndsWithSingleNewline operationalCertificateIssueCounterFile
