{-# LANGUAGE OverloadedStrings #-}

module Test.Golden.Sophie.Address.Build
  ( golden_sophieAddressBuild
  ) where

import           Bcc.Prelude
import           Hedgehog (Property)
import           Test.OptParse as OP

import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

{- HLINT ignore "Use camelCase" -}

golden_sophieAddressBuild :: Property
golden_sophieAddressBuild = propertyOnce . H.moduleWorkspace "tmp" $ \tempDir -> do
  addressVKeyFile <- noteInputFile "test/data/golden/sophie/keys/payment_keys/verification_key"
  addressSKeyFile <- noteInputFile "test/data/golden/sophie/keys/stake_keys/verification_key"
  goldenStakingAddressHexFile <- noteInputFile "test/data/golden/sophie/addresses/staking-address.hex"
  goldenEnterpriseAddressHexFile <- noteInputFile "test/data/golden/sophie/addresses/enterprise-address.hex"
  stakingAddressHexFile <- noteTempFile tempDir "staking-address.hex"
  enterpriseAddressHexFile <- noteTempFile tempDir "enterprise-address.hex"

  void $ H.readFile addressVKeyFile

  stakingAddressText <- execBccCLI
    [ "address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenStakingAddressHex <- H.readFile goldenStakingAddressHexFile

  H.writeFile stakingAddressHexFile stakingAddressText

  equivalence stakingAddressText goldenStakingAddressHex

  void $ H.readFile addressSKeyFile

  enterpriseAddressText <- execBccCLI
    [ "address","build"
    , "--testnet-magic", "14"
    , "--payment-verification-key-file", addressVKeyFile
    , "--staking-verification-key-file", addressSKeyFile
    ]

  goldenEnterpriseAddressHex <- H.readFile goldenEnterpriseAddressHexFile

  H.writeFile enterpriseAddressHexFile enterpriseAddressText

  equivalence enterpriseAddressText goldenEnterpriseAddressHex
