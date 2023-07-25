{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.StakeAddress.DeregistrationCertificate
  ( golden_sophieStakeAddressDeregistrationCertificate
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           System.FilePath ((</>))
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

{- HLINT ignore "Use camelCase" -}

golden_sophieStakeAddressDeregistrationCertificate :: Property
golden_sophieStakeAddressDeregistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  verificationKeyFile <- noteInputFile "test/data/golden/sophie/keys/stake_keys/verification_key"
  deregistrationCertFile <- noteTempFile tempDir "deregistrationCertFile"
  scriptDeregistrationCertFile <- noteTempFile tempDir "scripDeregistrationCertFile"
  exampleScript <- noteInputFile $ base </> "scripts/zerepoch/scripts/custom-guess-42-datum-42.zerepoch"

  void $ execBccCLI
    [ "stake-address","deregistration-certificate"
    , "--staking-verification-key-file", verificationKeyFile
    , "--out-file", deregistrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Deregistration Certificate" deregistrationCertFile

  void $ execBccCLI
    [ "stake-address","deregistration-certificate"
    , "--stake-script-file", exampleScript
    , "--out-file", scriptDeregistrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Deregistration Certificate" scriptDeregistrationCertFile

  H.assertEndsWithSingleNewline deregistrationCertFile
