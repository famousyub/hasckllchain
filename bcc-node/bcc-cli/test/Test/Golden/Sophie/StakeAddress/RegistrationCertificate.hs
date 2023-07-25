{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.StakeAddress.RegistrationCertificate
  ( golden_sophieStakeAddressRegistrationCertificate
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           System.FilePath ((</>))
import           Test.OptParse

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H
import qualified Hedgehog.Extras.Test.Process as H

{- HLINT ignore "Use camelCase" -}

golden_sophieStakeAddressRegistrationCertificate :: Property
golden_sophieStakeAddressRegistrationCertificate = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  base <- H.getProjectBase

  keyGenStakingVerificationKeyFile <- noteInputFile "test/data/golden/sophie/keys/stake_keys/verification_key"
  registrationCertFile <- noteTempFile tempDir "registration.cert"
  scriptRegistrationCertFile <- noteTempFile tempDir "script-registration.cert"
  exampleScript <- noteInputFile $ base </> "scripts/zerepoch/scripts/custom-guess-42-datum-42.zerepoch"

  void $ execBccCLI
    [ "stake-address","registration-certificate"
    , "--staking-verification-key-file", keyGenStakingVerificationKeyFile
    , "--out-file", registrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" registrationCertFile

  void $ execBccCLI
    [ "stake-address","registration-certificate"
    , "--stake-script-file", exampleScript
    , "--out-file", scriptRegistrationCertFile
    ]

  H.assertFileOccurences 1 "Stake Address Registration Certificate" scriptRegistrationCertFile

  H.assertEndsWithSingleNewline registrationCertFile
