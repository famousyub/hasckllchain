{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.StakePool.RegistrationCertificate
  ( golden_sophieStakePoolRegistrationCertificate
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieStakePoolRegistrationCertificate :: Property
golden_sophieStakePoolRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  operatorVerificationKeyFile <- noteInputFile "test/data/golden/sophie/node-pool/operator.vkey"
  vrfVerificationKeyFile <- noteInputFile "test/data/golden/sophie/node-pool/vrf.vkey"
  ownerVerificationKeyFile <- noteInputFile "test/data/golden/sophie/node-pool/owner.vkey"
  registrationCertFile <- noteTempFile tempDir "registration.cert"

  void $ execBccCLI
    [ "stake-pool","registration-certificate"
    , "--testnet-magic", "42"
    , "--pool-pledge", "0"
    , "--pool-cost", "0"
    , "--pool-margin", "0"
    , "--cold-verification-key-file", operatorVerificationKeyFile
    , "--vrf-verification-key-file", vrfVerificationKeyFile
    , "--reward-account-verification-key-file", ownerVerificationKeyFile
    , "--pool-owner-stake-verification-key-file", ownerVerificationKeyFile
    , "--out-file", registrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Pool Registration Certificate" registrationCertFile

  H.assertEndsWithSingleNewline registrationCertFile
